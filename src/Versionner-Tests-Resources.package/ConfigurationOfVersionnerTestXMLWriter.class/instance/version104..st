versions
version104: spec 
	<version: '1.0.4' imports: #('1.0.4-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.

		spec
			package: 'Collections-OrderPreservingDictionary'
			with: 'Collections-OrderPreservingDictionary-JAAyer.6';
			package: 'XML-Writer' with: 'XML-Writer-JAAyer.5'].

	spec for: #gemstone do: [
		spec 
			package: 'Collections-CharacterSet' with: 'Collections-CharacterSet-JohanBrichau.1']