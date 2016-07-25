baselines
baseline100: spec 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.

		spec 
			package: 'Collections-CharacterMap' with: [
				spec repository: 'http://www.squeaksource.com/CharacterMap'];
			package: 'XML-Writer' with: [spec requires: 'Collections-CharacterMap'].

		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer');
			group: 'Tests' with: #()].