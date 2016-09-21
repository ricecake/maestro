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
			MIDI.loadPlugin({
				soundfontUrl: "/static/soundfont/",
				instruments: [ 'acoustic_grand_piano', 'synth_drum' ],
				onprogress: function(state, progress) {
					console.log(state, progress);
				},
				onsuccess: function() {
					var delay = 0; // play one note every quarter second
					var note = 50; // the MIDI note
					var velocity = 127; // how hard the note hits
					// play the note
					MIDI.setVolume(0, 127);
					MIDI.noteOn(0, note, velocity, delay);
					MIDI.noteOff(0, note, delay + 0.75);
					connection.addEventHandlers({
						'note': function(key, content, raw){
							console.log('NOTE');
							MIDI.noteOn(0, content.note, velocity, delay);
							MIDI.noteOff(0, content.note, delay + 0.75);
						}
					});
				}
			});
		}
	});
});

});
