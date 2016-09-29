;define([
	'jquery',
	'KnotConn',
	'midi',
	'timbre'
], function($, KnotConn){
'use strict';
var needsSetup = true;

var instruments = [
	//"accordion",
	//"acoustic_bass",
	"acoustic_grand_piano",
	//"acoustic_guitar_nylon",
	//"acoustic_guitar_steel",
	//"agogo",
	//"alto_sax",
	//"applause",
	//"bagpipe",
	//"banjo",
	//"baritone_sax",
	//"bassoon",
	//"bird_tweet",
	//"blown_bottle",
	//"brass_section",
	//"breath_noise",
	"bright_acoustic_piano",
	//"celesta",
	//"cello",
	//"choir_aahs",
	//"church_organ",
	//"clarinet",
	//"clavinet",
	//"contrabass",
	//"distortion_guitar",
	//"drawbar_organ",
	//"dulcimer",
	"electric_bass_finger",
	//"electric_bass_pick",
	//"electric_grand_piano",
	//"electric_guitar_clean",
	//"electric_guitar_jazz",
	//"electric_guitar_muted",
	//"electric_piano_1",
	//"electric_piano_2",
	//"english_horn",
	//"fiddle",
	//"flute",
	//"french_horn",
	//"fretless_bass",
	//"fx_1_rain",
	//"fx_2_soundtrack",
	//"fx_3_crystal",
	//"fx_4_atmosphere",
	//"fx_5_brightness",
	//"fx_6_goblins",
	//"fx_7_echoes",
	//"fx_8_scifi",
	//"glockenspiel",
	//"guitar_fret_noise",
	//"guitar_harmonics",
	//"gunshot",
	//"harmonica",
	//"harpsichord",
	//"helicopter",
	//"honkytonk_piano",
	//"kalimba",
	//"koto",
	//"lead_1_square",
	//"lead_2_sawtooth",
	//"lead_3_calliope",
	//"lead_4_chiff",
	//"lead_5_charang",
	//"lead_6_voice",
	//"lead_7_fifths",
	//"lead_8_bass__lead",
	//"marimba",
	//"melodic_tom",
	//"music_box",
	//"muted_trumpet",
	//"oboe",
	//"ocarina",
	//"orchestra_hit",
	//"orchestral_harp",
	//"overdriven_guitar",
	"pad_1_new_age",
	//"pad_2_warm",
	//"pad_3_polysynth",
	//"pad_4_choir",
	//"pad_5_bowed",
	//"pad_6_metallic",
	//"pad_7_halo",
	//"pad_8_sweep",
	//"pan_flute",
	//"percussive_organ",
	//"piccolo",
	//"pizzicato_strings",
	//"recorder",
	//"reed_organ",
	//"reverse_cymbal",
	//"rock_organ",
	//"seashore",
	"shakuhachi",
	//"shamisen",
	//"shanai",
	//"sitar",
	//"slap_bass_1",
	//"slap_bass_2",
	//"soprano_sax",
	//"steel_drums",
	//"string_ensemble_1",
	//"string_ensemble_2",
	//"synth_bass_1",
	"synth_bass_2",
	//"synth_brass_1",
	//"synth_brass_2",
	//"synth_choir",
	//"synth_drum",
	//"synth_strings_1",
	//"synth_strings_2",
	//"taiko_drum",
	//"tango_accordion",
	//"telephone_ring",
	//"tenor_sax",
	//"timpani",
	//"tinkle_bell",
	//"tremolo_strings",
	//"trombone",
	//"trumpet",
	//"tuba",
	//"tubular_bells",
	//"vibraphone",
	//"viola",
	//"violin",
	//"voice_oohs",
	//"whistle",
	//"woodblock",
	//"xylophone"
];

$(document).ready(function(){
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
							console.log(raw, MIDI.GM.byId[content.program+1].instrument);
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

