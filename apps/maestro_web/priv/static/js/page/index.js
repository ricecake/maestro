;define([
	'jquery',
	'KnotConn',
	'midi',
	'timbre'
], function($, KnotConn){
'use strict';
var needsSetup = true;

var instruments = [
	"bright_acoustic_piano",
	"electric_bass_finger",
	"pad_1_new_age",
	"shakuhachi",
	"synth_bass_2"
];

$(document).ready(function(){
	//var instruments = [ 'acoustic_grand_piano', 'electric_bass_finger', 'synth_drum' ];
	var env = T("perc", {a:50, r:2500});
	var pluck = T("PluckGen", {env:env, mul:0.5}).play();
	MIDI.loadPlugin({
		soundfontUrl: "/static/soundfont/",
		instruments: instruments,
		onsuccess: function() {
			var delay = 0; // play one note every quarter second
			var note = 50; // the MIDI note
			var velocity = 127; // how hard the note hits
			// play the note
			MIDI.setVolume(0, 127);
			//MIDI.noteOn(0, note, velocity, delay);
			//MIDI.noteOff(0, note, delay + 0.75);

			console.log(MIDI.GM);
			//var i = 1;
			for(var i=0; i<16; i++) {
				MIDI.programChange(i, i);
			}
			//instruments.forEach(function(item, offset) {
			//	console.log(offset, item, offset, MIDI.GM.byName[item]);
			//	MIDI.programChange(offset*i, MIDI.GM.byName[item].number);
			//});
			//MIDI.programChange(0, MIDI.GM.byName['gunshot'].number);
			//MIDI.programChange(0, MIDI.GM.byName['banjo'].number);
			//MIDI.programChange(9, MIDI.GM.byName['gunshot'].number);
			//MIDI.programChange(0, MIDI.GM.byName['gunshot'].number);
			//MIDI.programChange(2, MIDI.GM.byName['acoustic_grand_piano'].number);
			//MIDI.programChange(9, MIDI.GM.byName['synth_drum'].number);
			var connection = new KnotConn({
				url: '/ws/',
				onOpen: function() {
					connection.addEventHandlers({
						'#': function(key, content, raw) {
							//console.log(raw);
						},
						'note.on': function(key, content, raw){
							//pluck.noteOn(content.note, content.velocity);
							MIDI.noteOn(content.channel, content.note, content.velocity, delay);
						},
						'note.off': function(key, content, raw){
							//pluck.noteOff(content.note, content.velocity);
							MIDI.noteOff(content.channel, content.note, delay);
						},
						'control.program': function(key, content, raw){
							//MIDI.programChange(content.channel, MIDI.GM.byName['gunshot'].number);
							MIDI.programChange(content.channel, content.program+1);
						},
					});
					connection.send('client.ready', {});
				}
			});
			//MIDI.Player.loadFile('/static/midi/ice_ice.mid', function(){ console.log('Loaded') });
		}
	});
});

});
