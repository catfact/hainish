Hainish_Chorus {
	classvar prSpecs;
	var <buf, <synth;
	var <inBus, <outBus;
	var <ctlBus, <ctlBusIdx;

	*specs {
		if (prSpecs.isNil, {
			prSpecs = Dictionary.newFrom([
				\lfoA: ControlSpec.new(1.0, 1.33, default:1.0),
				\lfoB: ControlSpec.new(0.23, 0.34, default:0.3),
				\lfoHz: ControlSpec.new(0.01, 100.0, \exponential, default:1.0),
				\modTime: ControlSpec.new(0.00004, 0.4, \exponential, default:0.01),
				\delMix: ControlSpec.new(0, 1, default:0.0),
				\decay: ControlSpec.new(0.00004, 1.0, \exponential, default:0.00004),
				\baseTime: ControlSpec.new(0.1, 0.8),
				\spread: ControlSpec.new(0, 1, default:1)
			]);
		});
		^prSpecs
	}

	// generate graph for a chaotically-modulated delay with a dedicated buffer
	*modDelay { arg in, buf, pan=0,
		decay=0, baseTime=0.02, minTime=0.002, maxTime=0.04, timeDriftAmt=0.002, timeDriftRate=0.01,
		lfoHz=1.0, modTime=0.01,
		lfoA=1.0, lfoB=0.24, lfoX0=0, lfoX1=0;

		var lfo, del, snd;
		buf.postln;

		lfo = HenonC.ar(lfoHz, lfoA, lfoB, lfoX0, lfoX1, mul:modTime, add:baseTime);
		// variations...
		//lfo = SinOsc.ar(lfoHz, mul:lfoMod*modTime, add:baseTime).min(maxTime).max(minTime);
		//lfo = LFTri.ar(lfoHz, mul:lfoMod*modTime, add:baseTime);
		//lfo = HenonL.ar(lfoHz, lfoA, lfoB, lfoX0, lfoX1, mul:lfoMod*modTime, add:baseTime);

		lfo = lfo + K2A.ar(LFNoise2.kr(timeDriftRate, timeDriftAmt)).lag(0.6);
		lfo = lfo.abs.min(maxTime).max(minTime);
		del = BufCombC.ar(buf, in, lfo, decay);
		// variations...
		//del = BufAllpassC.ar(buf, in, lfo, decay);
		snd = Pan2.ar(del, pan);
		^snd
	}

	*new {
		arg n, server, target, maxTime;
		^super.new.init(n, server, target, maxTime);
	}

	init {
		arg n, server, target, maxTime;
		var specs = this.class.specs;

		if (server.isNil, { server = Server.default; });
		if (target.isNil, { target = server; });
		if (maxTime.isNil, { maxTime = 1.0; });


		ctlBus = Bus.control(server, specs.size);
		ctlBusIdx = Dictionary.new;

		this.class.specs.keys.do({ arg k, i;
			ctlBusIdx[k] = ctlBus.index + i;
			ctlBus.subBus(i).set(specs[k].default);
		});

		buf = Buffer.alloc(server, maxTime * server.sampleRate + 8, n);
		inBus = Bus.audio(server, 2);
		outBus = Bus.audio(server, 2);

		synth = {
			var snd, ctl, del;
			var x0;

			snd = In.ar(inBus.index, 2);

			ctl = Dictionary.new;
			specs.keys.do({ arg k; ctl[k] = In.kr(ctlBusIdx[k]) });

			x0 = Array.fill(n,{ arg i; i/(n-1) * 0.2 });
			//x0 = Array.fill(n,{ 0 });

			del = Array.fill(n, { arg i;
				var x = i/(n-1);
				var xbi = x*2 - 1;
				//[i, x, xbi].poll;
				this.class.modDelay(
					in: SelectX.ar(x, snd),
					buf: buf.bufnum + i,
					pan: xbi * ctl[\spread],
					decay: ctl[\decay] ,
					baseTime: ctl[\baseTime],
					modTime: ctl[\modTime],
					minTime: 0.001,
					maxTime: 0.98,
					timeDriftAmt: 0.006,
					timeDriftRate: 0.004,
					lfoHz: ctl[\lfoHz],
					lfoA: ctl[\lfoA],
					lfoB: ctl[\lfoB],
					lfoX0: x0[i],
					lfoX1: x0.wrapAt(i+1)
				)
			});
			//del = Mix.new(del.flatten.clump(2));
			del = Mix.new(del);
			//del.poll;
			Out.ar(outBus.index, del);
		}.play(target:target);
	}

	setControl { arg k, val;
		ctlBus.subBus(ctlBusIdx[k]).set(val);
	}

	getControl { arg k;
		^ctlBus.subBus(ctlBusIdx[k]).getSynchronous;
	}

	free {
		ctlBus.do({ arg bus; bus.free; });
		synth.free;
		buf.free;
		inBus.free;
		outBus.free;
	}
}

Hainish_VoiceOsc {
}

Hainish_BassOsc {
}

Hainish {
}