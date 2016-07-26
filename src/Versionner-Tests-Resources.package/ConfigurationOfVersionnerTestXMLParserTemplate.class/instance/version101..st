versions
version101: spec
	<version: '1.1' imports: #('1.0-baseline')>
	spec
		for: #common
		do: [ 
			spec blessing: #release.
			spec
				package: 'VersionnerTestXML-Parser' with: 'VersionnerTestXML-Parser-StephaneDucasse.142';
				package: 'VersionnerTestXML-Tests-Parser' with: 'VersionnerTestXML-Tests-Parser-StephaneDucasse.15'. ].
	
	