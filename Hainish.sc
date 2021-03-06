

/////////////////
/// full instrument
Hainish {
	classvar <moduleKeys;
	var <numUpperVoices, <numBassVoices, <maxDelayTime;
	var <server, <sources, <sinks, <groups, <patches;
	var <modules, <controls, <specs;

	*new {
		^super.new.init;
	}

	*initClass {
		moduleKeys = [
			\lfo, \bass, \voice, \resonator, \delay
		];
	}

	init {
		var addPatch;

		numUpperVoices = 8;
		numBassVoices = 1;
		maxDelayTime = 16; // why not

		server = Server.default;

		groups = Dictionary.new;
		sources = Dictionary.new;
		sinks = Dictionary.new;
		patches = Dictionary.new;

		modules = Dictionary.new;
		controls = Dictionary.new;

		moduleKeys.do({ arg k;
			groups[k] = Group.new(server, addAction:\addToTail);
		});

		// modulation source
		modules[\lfo] = Hainish_Lfo.new(server, groups[\lfo], numUpperVoices.max(numBassVoices));
		controls[\lfo] = modules[\lfo].ctlBus;

		sources[\voice] = Bus.audio(server, 2);
		sources[\bass]  = Bus.audio(server, 2);
		// voice modules
		// these are arrays which share control bus data (a Dictionary)
		// so we construct the busses first (using non-obvious factory class methods)
		controls[\voice] = Hainish_VoiceOsc.controls;
		controls[\bass]  = Hainish_BassOsc.controls;

		modules[\bass] = Array.fill(numBassVoices, { arg i;
			var busIdx = modules[\lfo].outBus.subBus(i).index;
			busIdx.postln;
			Hainish_BassOsc.new(groups[\bass], sources[\bass], controls[\bass], busIdx)
		});
		modules[\voice] = Array.fill(numUpperVoices, { arg i;
			var busIdx = modules[\lfo].outBus.subBus(i).index;
			busIdx.postln;
			Hainish_VoiceOsc(groups[\voice], sources[\voice], controls[\voice], busIdx)
		});

		// FX modules
		// don't share, so each instance constructs its own control bus dict
		modules[\resonator]  = Hainish_Resonator(server, groups[\resonator]);
		modules[\delay]     = Hainish_Delay(server, groups[\delay], 3, 16.0);
		controls[\resonator] = modules[\resonator].ctlBus;
		controls[\delay]    = modules[\delay].ctlBus;

		sources[\resonator] = modules[\resonator].outBus;
		sources[\delay]    = modules[\delay].outBus;

		sinks[\resonator] = modules[\resonator].inBus;
		sinks[\delay]    = modules[\delay].inBus;
		sinks[\output]     = Bus.audio(server, 2);

		// patch points, and a dict of level controls
		controls[\patches] = Dictionary.new;
		addPatch = { arg src, dst, scale=1.0;
			var k = (src.asString ++ "_" ++ dst.asString).asSymbol;
			controls[\patches][k] = Bus.control(server, 1);
			controls[\patches][k].set(1.0);
			patches[k] = {
				Out.ar(sinks[dst].index,
					In.kr(controls[\patches][k].index).lag(0.1) * In.ar(sources[src].index, 2) * scale);
			}.play(target:groups[src], addAction:\addAfter);
		};

		addPatch.value(\voice,     \resonator, 1.0 / numUpperVoices);
		addPatch.value(\resonator, \delay);
		addPatch.value(\bass,      \delay, 0.2);
		addPatch.value(\bass,      \output, 0.2);
		addPatch.value(\resonator, \output, 0.6);
		addPatch.value(\delay,    \output, 1.0);

		//------------------------------
		/// "delay" feedback
		//// FIXME: should just put this in the delay module...

		controls[\delayFb] = Dictionary.new;
		controls[\delayFb][\amt] = Bus.control(server, 1).set(0.6);
		controls[\delayFb][\lpfFc] = Bus.control(server, 1).set(7500);
		controls[\delayFb][\hpfFc] = Bus.control(server, 1).set(80);

		patches[\delay_feedback] = {
			var snd, amp, lpfFc, hpfFc;
			amp = In.kr(controls[\delayFb][\amt]).lag(0.1);
			lpfFc = In.kr(controls[\delayFb][\lpfFc]).lag(0.1);
			hpfFc = In.kr(controls[\delayFb][\hpfFc]).lag(0.1);
			snd = InFeedback.ar(sources[\delay], 2);
			snd = HPF.ar(LPF.ar(snd, lpfFc), hpfFc);
			Out.ar(sinks[\delay], snd * amp);
		}.play(groups[\delay], addAction:\addToHead);

		//------------------------------


		patches[\output_system] = { Out.ar(0, In.ar(sinks[\output], 2)) }.play(server, addAction:\addToTail);

	}
}

//////////////////////
/// delay module
Hainish_Delay {
	classvar prSpecs;
	var <bufs, <synth;
	var <inBus, <outBus;
	var <ctlBus, <ctlBusIdx;

	*specs {
		if (prSpecs.isNil, {
			prSpecs = Dictionary.newFrom([
				\lfoA: ControlSpec.new(1.0, 1.33, default:1.0),
				\lfoB: ControlSpec.new(0.23, 0.34, default:0.3),
				\lfoHz: ControlSpec.new(0.01, 100.0, \exponential, default:4),
				\modTime: ControlSpec.new(0.00004, 0.4, \exponential, default:0.002),
				\decay: ControlSpec.new(0.00004, 20.0, \exponential, default:0.04),
				\baseTime: ControlSpec.new(0.1, 0.8, default:0.1),
				\spread: ControlSpec.new(0, 1, default:1)
			]);
		});
		^prSpecs
	}

	*new {
		arg server, target, n, maxTime;
		^super.new.init(server, target, n, maxTime);
	}

	init {
		arg server, target, n, maxTime;
		var specs = this.class.specs;

		if (server.isNil, { server = Server.default; });
		if (target.isNil, { target = server; });
		if (maxTime.isNil, { maxTime = 1.0; });


		ctlBus = Bus.control(server, specs.size);
		ctlBusIdx = Dictionary.new;

		this.class.specs.keys.do({ arg k, i;
			//[k, i].postln;
			ctlBusIdx[k] = ctlBus.index + i;
			ctlBus.subBus(i).set(specs[k].default);
		});

		bufs = Buffer.allocConsecutive(n, server, maxTime * server.sampleRate + 8, 1);
		inBus = Bus.audio(server, 2);
		outBus = Bus.audio(server, 2);

		synth = {
			var snd, ctl, del;
			var x0;

			snd = In.ar(inBus.index, 2);

			ctl = Dictionary.new;
			specs.keys.do({ arg k; ctl[k] = In.kr(ctlBusIdx[k]) });

			x0 = Array.fill(n,{ arg i; i/(n-1) * 0.2 });

			del = Array.fill(n, { arg i;
				var x = i/(n-1);
				var xbi = x*2 - 1;
				var xs = (i%2) * 2 - 1;
				var in = SelectX.ar(x, snd);
				var buf = bufs[i].bufnum;
				var pan = (xbi * xs * ctl[\spread]).min(1).max(-1);
				var decay = ctl[\decay];
				var baseTime = ctl[\baseTime];
				var modTime = ctl[\modTime];
				var minTime = 0.001;
				var maxTime = 0.98;
				var timeDriftAmt = 0.008;
				var timeDriftRate = 0.02;
				var lfoHz = ctl[\lfoHz];
				var lfoA = ctl[\lfoA];
				var lfoB = ctl[\lfoB];
				var lfoX0 = x0[i];
				var lfoX1 = x0.wrapAt(i+1);

				var lfo, del, modScale;

				modScale = 1/lfoHz.max(0.0001);

				//lfo = HenonL.ar(lfoHz, lfoA, lfoB, lfoX0, lfoX1, mul:modTime, add:baseTime);
				//lfo = SinOsc.ar(lfoHz, lfoA, mul:modTime, add:baseTime);
				//lfo = LFTri.ar(lfoHz, lfoA, mul:modTime, add:baseTime);
				lfo = HenonC.ar(lfoHz, lfoA, lfoB, lfoX0, lfoX1, mul:modTime*modScale, add:baseTime);
				del = BufCombC.ar(buf, in, lfo, decay);
				//del = BufAllpassC.ar(buf, in, lfo, decay);

				Pan2.ar(del / n, pan)
			});
			del = Mix.new(del);
			Out.ar(outBus.index, del);
		}.play(target:target);
	}

	setControlRaw { arg k, val;
		var busIdxOffset = ctlBusIdx[k] - ctlBus.index;
		ctlBus.subBus(busIdxOffset).set(val);
	}

	setControlMapped { arg k, val;
		var busIdxOffset = ctlBusIdx[k] - ctlBus.index;
		var mappedVal = this.class.specs[k].map(val);
		ctlBus.subBus(busIdxOffset).set(val);
	}

	getControlValue { arg k;
		^ctlBus.subBus(ctlBusIdx[k]).getSynchronous;
	}


	free {
		ctlBus.do({ arg bus; bus.free; });
		synth.free;
		bufs.do({ arg buf; buf.free; });
		inBus.free;
		outBus.free;
	}
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Hainish_VoiceOsc
// single upper oscillator synth

Hainish_VoiceOsc {
	classvar prSpecs;
	var <synth;

	*specs {
		if (prSpecs.isNil, {
			prSpecs = Dictionary.newFrom([
				\aaFc, ControlSpec.new(4000, 16000, \exponential, units:\Hz, default:8000),
				\shape, ControlSpec.new(0.999, 0.001, \exponential),
				\detune, ControlSpec.new(0, 100, units:\cents, default:4),
				\spread, ControlSpec.new(0, 1, default:1),
				\modTune, ControlSpec.new(0, 100, units:\cents, default:0),
				\modAmp, ControlSpec.new(0, 1, default:0),

				\attack, ControlSpec.new(0.001, 16,\exponential, units:\s, default:1.2),
				\decay, ControlSpec.new(0.001, 16,\exponential, units:\s, default:1.1),
				\sustain, ControlSpec.new(0, 1, default:0.6),
				\release, ControlSpec.new(0.001, 16,\exponential, units:\s, default:2.0)
			]);
		});
		^prSpecs
	}

	// generate a set of control busses
	*controls { arg server;
		var ctl = Dictionary.new;
		Hainish_VoiceOsc.specs.keys.do({ arg k;
			var val = Hainish_VoiceOsc.specs[k].default;
			ctl[k] = Bus.control(server, 1);
			ctl[k].set(val);
		});
		^ctl;
	}

	// `controls` should be a structure returned by *controls
	*new { arg target, output, controls, modIn;
		^super.new.init(target, output, controls, modIn);
	}


	// `controls` should be a structure returned by *controls
	init { arg target, output, controls, modIn;

		// FIXME: def compilation should be memoized
		synth = {
			arg gate=0, hz=110, fastGate=1;
			var osc, aosc, aenv, snd;
			var mod, amp, ampMod, tuneMod;
			var ctl;
			var a;

			mod = In.ar(modIn);

			ctl = Dictionary.new;
			controls.keys.do({ arg k;
				ctl[k] = In.kr(controls[k].index).lag(0.1);
			});

			aenv = EnvGen.ar(
				Env.adsr(ctl[\attack], ctl[\decay], ctl[\sustain], ctl[\release]),
				gate
			);
			aenv = aenv * EnvGen.ar(Env.asr(0, 1, 0.01), fastGate);

			tuneMod = mod * ctl[\modTune] * 0.01;
			osc = Saw.ar(hz * (tuneMod + (ctl[\detune] * [-0.01, 0.01])).midiratio);
			osc = LPF.ar(osc, ctl[\aaFc]);
			aosc = osc.abs;
			a = ctl[\shape];
			osc = osc * (aosc + a) / (osc * osc + (a-1) * aosc + 1);

			snd = Pan2.ar(osc[0], -1 * ctl[\spread]) +  Pan2.ar(osc[1], ctl[\spread]);

			ampMod = mod*0.5 + 0.5;
			amp = SelectX.ar(ctl[\modAmp], [aenv, aenv*ampMod]);
			snd = snd * amp;

			Out.ar(output, snd);
		}.play(target:target);
	}

	play { arg hz;
		synth.set(\hz, hz);
		synth.set(\gate, 1);
		synth.set(\fastGate, 1);
	}

	release {
		synth.set(\gate, 0);
	}

	stop {
		synth.set(\fastGate, 0);
	}
}

//////////////////////
/// lower oscillator synth
Hainish_BassOsc {
classvar prSpecs;
	var <synth;

	*specs {
		if (prSpecs.isNil, {
			prSpecs = Dictionary.newFrom([
				\pulseLevel, ControlSpec.new(0, 1, default:0.5),
				\fmLevel, ControlSpec.new(0, 1, default:0.5),

				\pulseRatio, ControlSpec.new(0.25, 1, \exponential, default:0.25),
				\fmCarRatio, ControlSpec.new(0.25, 2, \exponential, default:0.5),
				\fmModRatio, ControlSpec.new(0.125, 8, \exponential, default:1.5),

				\fmModAmt, ControlSpec.new(0, 1, default:1),
				\pulseWidth, ControlSpec.new(0.01, 0.99, default:0.5),

				\aaFc, ControlSpec.new(4000, 16000, \exponential, units:\Hz, default:8000),
				\fmShape, ControlSpec.new(0, 1, default:0),

				\lpfFcRatio, ControlSpec.new(1, 16, \exponential, default:4),
				\lpfFcMod, ControlSpec.new(0, 1, default:0),
				\lpfGain, ControlSpec.new(0, 4, default:1),

				\attack, ControlSpec.new(0.001, 16,\exponential, units:\s, default:0.2),
				\decay, ControlSpec.new(0.001, 16,\exponential, units:\s, default:1.1),
				\sustain, ControlSpec.new(0, 1, default:1.0),
				\release, ControlSpec.new(0.001, 16,\exponential, units:\s, default:2.0)
			]);
		});
		^prSpecs
	}

	// generate a set of control busses
	*controls { arg server;
		var ctl = Dictionary.new;
		Hainish_BassOsc.specs.keys.do({ arg k;
			var val = Hainish_BassOsc.specs[k].default;
			ctl[k] = Bus.control(server, 1);
			ctl[k].set(val);
		});
		^ctl;
	}

	// `controls` should be a structure returned by *controls
	*new { arg target, output, controls, modIn;
		^super.new.init(target, output, controls, modIn);
	}

	// `controls` should be a structure returned by *controls
	init { arg target, output, controls, modIn;

		synth = {
			arg gate=0, hz=110, fastGate=1;
			var pulse, osc, aosc, aenv, snd;
			var ctl;
			var a;
			var fc;

			ctl = Dictionary.new;
			controls.keys.do({ arg k;
				ctl[k] = In.kr(controls[k].index).lag(0.1);
			});

			aenv = EnvGen.ar(Env.adsr(ctl[\attack], ctl[\decay], ctl[\sustain], ctl[\release]),gate);
			aenv = aenv * EnvGen.ar(Env.asr(0, 1, 0.01), fastGate);

			osc = PMOsc.ar(hz * ctl[\fmCarRatio], hz * ctl[\fmModRatio], ctl[\fmModAmt]);

			osc = LPF.ar(osc, ctl[\aaFc]);
			aosc = osc.abs;
			a = ctl[\fmShape];
			osc = osc * (aosc + a) / (osc * osc + (a-1) * aosc + 1) * ctl[\fmLevel];
			snd = osc + Pulse.ar(hz * ctl[\pulseRatio], ctl[\pulseWidth], ctl[\pulseLevel]);

			fc = K2A.ar((ctl[\lpfFcRatio]*hz).min(12000));
			fc = SelectX.ar(ctl[\lpfFcMod], [fc, (fc * In.ar(modIn)*0.5 + 0.5).max(10)]);

			snd = MoogFF.ar(snd, fc, ctl[\lpfGain]);
			snd = snd * aenv;

			Out.ar(output, snd.dup);
		}.play(target:target);
	}

	play { arg hz;
		synth.set(\hz, hz);
		synth.set(\gate, 1);
		synth.set(\fastGate, 1);
	}

	release {
		synth.set(\gate, 0);
	}

	stop {
		synth.set(\fastGate, 0);
	}
}

///////////////////
/// bank of 3 multimode filters ("upper resonator" section)
Hainish_Resonator {
	classvar prSpecs;
	var <synth, <inBus, <outBus;
	var <ctlBus, <ctlBusIdx;

	*specs {
		if (prSpecs.isNil, {
			var gainDbSpec = ControlSpec.new(-6.0, 6.0, default:0, units:\dB);
			var satSpec = ControlSpec.new(0.01, 0.95, default:0.95);
			prSpecs = Dictionary.newFrom([
				\mode1, ControlSpec.new(0, 2, default:0, step:1),
				\fc1,   ControlSpec.new(30, 1800, \exponential, default:150, units:\Hz),
				\q1,    ControlSpec.new(0.0, 1.0, default:0.0),
				\gain1, gainDbSpec,
				\sat1,  satSpec,

				\mode2, ControlSpec.new(0, 2, default:2, step:1),
				\fc2,   ControlSpec.new(150, 3000, \exponential, default:900, units:\Hz),
				\q2,    ControlSpec.new(0.0, 1.0, default:0.0),
				\gain2, gainDbSpec,
				\sat2,  satSpec,

				\mode3, ControlSpec.new(0, 2, default:1, step:1),
				\fc3,   ControlSpec.new(900, 9000, \exponential, default:3000, units:\Hz),
				\q3,    ControlSpec.new(0.0, 1.0, default:0.0),
				\gain3, gainDbSpec,
				\sat3,  satSpec,
			]);
		});
		//prSpecs.postln;
		^prSpecs
	}

	*new {
		arg server, target;
		^super.new.init(server, target);
	}

	init {
		arg server, target;

		var specs = this.class.specs;

		if (server.isNil, { server = Server.default; });
		if (target.isNil, { target = server; });

		ctlBus = Bus.control(server, specs.size);
		ctlBusIdx = Dictionary.new;

		this.class.specs.keys.do({ arg k, i;
			//[k, i].postln;
			ctlBusIdx[k] = ctlBus.index + i;
			ctlBus.subBus(i).set(specs[k].default);
		});

		inBus = Bus.audio(server, 2);
		outBus = Bus.audio(server, 2);

		synth = {
			var input, ctl, snd;
			input = In.ar(inBus, 2);
			ctl = Dictionary.new;
			specs.keys.do({ arg k; ctl[k] = In.kr(ctlBusIdx[k]).lag(0.1) });

			snd = BMoog.ar(input, ctl[\fc1], ctl[\q1], ctl[\mode1], ctl[\sat1], ctl[\gain1].dbamp)
				+ BMoog.ar(input, ctl[\fc2], ctl[\q2], ctl[\mode2], ctl[\sat2], ctl[\gain2].dbamp)
				+ BMoog.ar(input, ctl[\fc3], ctl[\q3], ctl[\mode3], ctl[\sat3], ctl[\gain3].dbamp);

			Out.ar(outBus.index, snd*0.3);

		}.play(target);
	}


	setControlRaw { arg k, val;
		var busIdxOffset = ctlBusIdx[k] - ctlBus.index;
		ctlBus.subBus(busIdxOffset).set(val);
	}

	setControlMapped { arg k, val;
		var busIdxOffset = ctlBusIdx[k] - ctlBus.index;
		var mappedVal = this.class.specs[k].map(val);
		ctlBus.subBus(busIdxOffset).set(val);
	}

}

Hainish_Lfo {
classvar prSpecs;
	var <synth, <outBus;
	var <ctlBus, <ctlBusIdx;

	*specs {
		if (prSpecs.isNil, {
			var gainDbSpec = ControlSpec.new(-6.0, 6.0, default:0, units:\dB);
			var satSpec = ControlSpec.new(0.01, 0.95, default:0.95);
			prSpecs = Dictionary.newFrom([
				\hz, ControlSpec(0.01, 100, default:1.2, units:\Hz),
				\shape, ControlSpec(0, 1, default:0),
				\phaseSpread, ControlSpec(0, 1, default:1)
			]);
		});
		//prSpecs.postln;
		^prSpecs
	}

	*new {
		arg server, target, n;
		^super.new.init(server, target, n);
	}

	init {
		arg server, target, n;

		var specs = this.class.specs;

		if (server.isNil, { server = Server.default; });
		if (target.isNil, { target = server; });

		ctlBus = Bus.control(server, specs.size);
		ctlBusIdx = Dictionary.new;

		specs.keys.do({ arg k, i;
			[k, i].postln;
			ctlBusIdx[k] = ctlBus.index + i;
			ctlBus.subBus(i).set(specs[k].default);
		});

		outBus = Bus.audio(server, n);

		synth = {
			var ctl, lfo;
			ctl = Dictionary.new;
			ctlBusIdx.keys.do({ arg k; ctl[k] = In.kr(ctlBusIdx[k]).lag(0.1); });
			lfo = Array.fill(n, {|i| SinOsc.ar(ctl[\hz], i.linlin(0, n-1, 0, pi) * ctl[\phaseSpread])});
			Out.ar(outBus.index, lfo);
		}.play(target);
	}


	setControlRaw { arg k, val;
		var busIdxOffset = ctlBusIdx[k] - ctlBus.index;
		ctlBus.subBus(busIdxOffset).set(val);
	}

	setControlMapped { arg k, val;
		var busIdxOffset = ctlBusIdx[k] - ctlBus.index;
		var mappedVal = this.class.specs[k].map(val);
		ctlBus.subBus(busIdxOffset).set(val);
	}

}

//// TODO:
// refactoring 