versions
version103: spec 
	<version: '1.0.3' imports: #('1.0.1-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-OrderPreservingDictionary'
			with: 'Collections-OrderPreservingDictionary-JAAyer.5';
			package: 'Collections-CharacterMap' with: 'Collections-CharacterMap-JAAyer.1';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.4'].