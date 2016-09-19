requirejs.config({
	baseUrl: 'static/js',
	paths: {
		jquery: 'lib/ext/jquery-2.1.1.min',
		underscore: 'lib/ext/underscore-min',
		KnotConn: 'lib/ext/connection',
		text: 'lib/ext/text',
		tpl: 'lib/ext/tpl',
		midi: 'lib/ext/MIDI.min',
		template: '../template'
	},
	shim: {
		midi:{
			deps: [
				'lib/ext/Base64binary'
			]
		}
	}
});
