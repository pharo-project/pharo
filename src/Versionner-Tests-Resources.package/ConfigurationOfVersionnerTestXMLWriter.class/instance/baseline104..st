baselines
baseline104: spec 
	<version: '1.0.4-baseline'>
	
	spec for: #common do: [
		spec
			blessing: #baseline;
			repository: 'http://www.squeaksource.com/XMLWriter'.

		spec
			package: 'Collections-OrderPreservingDictionary' with: [
				spec repository: 'http://www.squeaksource.com/OrderPreservingDict'];
			package: 'XML-Writer' with: [
				spec requires: #('Collections-OrderPreservingDictionary')].

		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer')].
		
	spec for: #gemstone do: [
		spec
			package: 'Collections-CharacterSet'.
		spec 
			package: 'XML-Writer' with:[spec requires: #('Collections-CharacterSet')]]