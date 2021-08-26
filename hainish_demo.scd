// WIP demo script of the `Hainish` "synth kernel"

h = Hainish.new;
h.sinks[\output].scope;

MIDIIn.connectAll;

//-----------------
//--- tuning

/* // standard
t = { arg midi; midi.midicps };
*/

// JI A440
~tdata = (
	baseMidi: 69,
	baseHz: 440,
	scale: [
		1, (4/3)/(5/4), 9/8,
		6/5, 5/4,
		4/3, (6/5)*(6/5), 3/2,
		(4/3)*(6/5), (4/3)*(5/4),
		(3/2)*(6/5), (3/2)*(5/4)
	]
);

t = { arg midi;
	var deg = midi - ~tdata.baseMidi;
	var oct = (deg/12).floor;
	var rat = ~tdata.scale[deg%12];
	~tdata.baseHz * rat * (2 ** oct);
};


//--------------------------
///--- voice allocation

~upperHeld = OrderedIdentitySet.new;
~upperAvail = OrderedIdentitySet.new;
~upperNotes = Array.newClear(h.numUpperVoices);
h.numUpperVoices.do({|i| ~upperAvail.add(i) });
~upperAvail.postln;

~notesHeld = OrderedIdentitySet.new;

~bassHeld = nil;

~upperNoteOn = { arg num, vel;
	var voice, stolen;

	//postln("allocating upper voice; avail = " ++ ~upperAvail);
	voice = ~upperAvail.asArray[0];
	~upperAvail.remove(voice);

	if (voice.isNil, { // stealing:
		//postln("stealing...");
		voice = ~upperHeld.array[0];
		~upperHeld.remove(voice);
	});

	//postln("(note on) upper voice index: " ++ voice);
	if (voice.isNil, {
		postln("!! error !! : nil voice index in upperNoteOn");
	}, {
		h.modules[\voice][voice].play(t.value(num));
		~upperHeld.add(voice);
		~upperNotes[voice] = num;
	});
};

~upperNoteOff = { arg num, vel;
	var voice = ~upperNotes.indexOfEqual(num);
	//postln("(note off) upper voice index: " ++ voice);
	if (voice.isNil, {
		postln("!! error !! : nil voice index in upperNoteOff");
	}, {
		h.modules[\voice][voice].release;
		~upperHeld.remove(voice);
		~upperAvail.add(voice);
		~upperNotes[voice] = nil;
	});
};

~bassNoteOn = { arg num, vel;
	//	~notesHeld.postln;
	var isBass = (~notesHeld.size < 1) || (num <= ~notesHeld.asArray[0]);
	if (isBass, {
		postln("iz bazz: "++num);
		h.modules[\bass][0].play(t.value(num));
		~bassHeld = num;
	});
};

~bassNoteOff = { arg num, vel;
	if (num == ~bassHeld, {
		h.modules[\bass][0].release;
		~bassHeld = nil;
	});
};

//------------------
//--- MIDI glue

MIDIdef.noteOn(\hainish_noteon, {arg vel, num;
	[vel, num].postln;
	~notesHeld.add(num);
	~notesHeld.sort;
	~upperNoteOn.value(num, vel);
	~bassNoteOn.value(num, vel);
});

MIDIdef.noteOff(\hainish_noteoff, {arg vel, num;
	//[vel, num].postln;
	~notesHeld.remove(num);
	~upperNoteOff.value(num);
	~bassNoteOff.value(num);
});

//.. add CC mapping..

//-----------------
//--- test UI

~gui_row_bounds = 444@28;

w = Window.new("hainish", Rect(100, Window.screenBounds.height-700, 444, 666), scroll:true);
q = w.addFlowLayout(10@10,20@5);
/*
FlowLayout
*/
StaticText(w, ~gui_row_bounds ).background_(Color.gray).string_("UPPER OSC");
~voiceSpecs = Hainish_VoiceOsc.specs;
~voiceSpecs.keys.do ({ arg k;
	EZSlider(w, ~gui_row_bounds, k, ~voiceSpecs[k], { arg sl;
		[k, sl.value].postln;
		h.controls[\voice][k].set(sl.value);
	});
});

StaticText(w, ~gui_row_bounds ).background_(Color.gray).string_("BASS OSC");
~bassSpecs = Hainish_BassOsc.specs;
~bassSpecs.keys.do ({ arg k;
	EZSlider(w, ~gui_row_bounds , k, ~bassSpecs[k], { arg sl;
		[k, sl.value].postln;
		h.controls[\bass][k].set(sl.value);
	});
});

StaticText(w, ~gui_row_bounds).background_(Color.gray).string_("RESONATOR");
~resoSpecKeys=
[
	\mode1,
	\gain1,
	\fc1,
	\q1,
	\sat1,
	\mode2,
	\gain2,
	\fc2,
	\q2,
	\sat2,
	\mode3,
	\gain3,
	\fc3,
	\q3,
	\sat3,
];//.do({ arg k; ~resoSpecs[k] = Hainish_Resonator.specs[k] });
~resoSpecKeys.do ({ arg k;
	EZSlider(w, ~gui_row_bounds, k,  Hainish_Resonator.specs[k], { arg sl;
		[k, sl.value].postln;
		h.modules[\resonator].setControlRaw(k, sl.value);
	});
});

StaticText(w, ~gui_row_bounds).background_(Color.gray).string_("CHORUS");
~delaySpecs = Hainish_Delay.specs;
~delaySpecs.keys.do ({ arg k;
	EZSlider(w, ~gui_row_bounds, k, ~delaySpecs[k], { arg sl;
		[k, sl.value].postln;
		h.modules[\delay].setControlRaw(k, sl.value);
	});
});

StaticText(w, ~gui_row_bounds).background_(Color.gray).string_("LEVELS");
~levelCtl = h.controls[\patches];
~levelCtl.keys.do ({ arg k;
	EZSlider(w, ~gui_row_bounds, k, ControlSpec(-60, 0), { arg sl;
		[k, sl.value].postln;
		~levelCtl[k].set(sl.value.dbamp);
	});
});


// extra feedback controls (FIXME, shouldn't need special treatment)
~delayFbCtl = h.controls[\delayFb];
EZSlider(w, ~gui_row_bounds, \fbAmt, ControlSpec(-60, 0), { arg sl; ~delayFbCtl[\amt].set(sl.value.dbamp) });
EZSlider(w, ~gui_row_bounds, \fbLfpFc, ControlSpec(10, 12000, warp:\exponential, default:8000),
	{ arg sl; ~delayFbCtl[\lpfFc].set(sl.value) });
EZSlider(w, ~gui_row_bounds, \fbHpfFc, ControlSpec(10, 12000, warp:\exponential, default:200),
	{ arg sl; ~delayFbCtl[\hpfFc].set(sl.value) });



w.front;

/*
w.close;
*/