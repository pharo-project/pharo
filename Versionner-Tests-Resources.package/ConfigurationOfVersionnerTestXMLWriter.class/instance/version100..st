versions
version100: spec 
	<version: '1.0' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-CharacterMap' with: 'Collections-CharacterMap-JAAyer.1';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.1'].