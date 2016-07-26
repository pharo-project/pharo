baselines
baseline101: spec 
	<version: '1.0.1-baseline'>
	
	spec for: #common do: [
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.

		spec
			package: 'Collections-OrderPreservingDictionary' with: [
				spec repository: 'http://www.squeaksource.com/OrderPreservingDict'];
			package: 'Collections-CharacterMap' with: [
				spec repository: 'http://www.squeaksource.com/CharacterMap'];
			package: 'XML-Writer' with: [
				spec requires: #('Collections-OrderPreservingDictionary' 'Collections-CharacterMap')].

		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer')].