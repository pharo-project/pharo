(CITask named: 'pharoFull')
		addSubtask: CIExamples metacello;
		command: 'ln -s . pharo-core'; "Required for the correct work of metacello baselines"
		dump: 'src';
		dump: 'resources/fonts/BitmapDejaVuSans.fuel.zip';
		command: 'unzip BitmapDejaVuSans.fuel.zip';
		command: 'mkdir icon-packs';
		command: 'cd icon-packs';
		command: 'wget http://github.com/pharo-project/pharo-icon-packs/archive/idea11.zip';
		command: 'cd ..';
		pharoEval: 'Metacello new baseline: ''IDE'';repository: ''filetree://src''; load'
			save: true;
		yourself
