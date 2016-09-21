;define([
	'jquery',
	'KnotConn',
	'midi'
], function($, KnotConn){
'use strict';
var needsSetup = true;

$(document).ready(function(){
        var connection = new KnotConn({
		url: '/ws/',
		onOpen: function() {
			console.log("Here!");
			var instruments = [ 'acoustic_grand_piano', 'electric_bass_finger', 'synth_drum' ];
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
					instruments.forEach(function(item, offset) {
						MIDI.programChange(offset, MIDI.GM.byName[item].number);
					});
					connection.addEventHandlers({
						'note': function(key, content, raw){
							console.log('NOTE', MIDI.GM);
							console.log(MIDI.getInstrument(0));
							MIDI.noteOn(content.channel, content.note, velocity, delay);
							MIDI.noteOff(content.channel, content.note, delay + 0.75);
						}
					});
				}
			});
		}
	});
});

});
