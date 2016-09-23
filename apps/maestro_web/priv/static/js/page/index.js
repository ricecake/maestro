;define([
	'jquery',
	'KnotConn',
	'midi',
	'timbre'
], function($, KnotConn){
'use strict';
var needsSetup = true;

$(document).ready(function(){
        var connection = new KnotConn({
		url: '/ws/',
		onOpen: function() {
			//var instruments = [ 'acoustic_grand_piano', 'electric_bass_finger', 'synth_drum' ];
			var env = T("perc", {a:50, r:2500});
			var pluck = T("PluckGen", {env:env, mul:0.5}).play();
			MIDI.loadPlugin({
				soundfontUrl: "/static/soundfont/",
				instruments: instruments,
				onprogress: function(state, progress) {
					console.log(state, progress);
				},
				onsuccess: function() {
					var delay = 0; // play one note every quarter second
					var note = 50; // the MIDI note
					var velocity = 127; // how hard the note hits
					// play the note
					MIDI.setVolume(0, 127);
					//MIDI.noteOn(0, note, velocity, delay);
					//MIDI.noteOff(0, note, delay + 0.75);
					for (var i = 1; i < 8; i++) {
					instruments.forEach(function(item, offset) {
						MIDI.programChange(offset*i, MIDI.GM.byName[item].number);
					});
					}
					MIDI.programChange(2, MIDI.GM.byName['acoustic_grand_piano'].number);
					MIDI.programChange(9, MIDI.GM.byName['synth_drum'].number);
					connection.addEventHandlers({
						'#': function(key, content, raw) {
							console.log(raw);
						},
						'note.on': function(key, content, raw){
							MIDI.noteOn(content.channel, content.note, content.velocity, delay);
						},
						'note.off': function(key, content, raw){
							MIDI.noteOff(content.channel, content.note, delay);
						},
						'control.program': function(key, content, raw){
							MIDI.programChange(content.channel, content.program);
						},
					});
					MIDI.Player.loadFile('/static/midi/ice_ice.mid', function(){ console.log('Loaded') });
				}
			});
		}
	});
});

});
