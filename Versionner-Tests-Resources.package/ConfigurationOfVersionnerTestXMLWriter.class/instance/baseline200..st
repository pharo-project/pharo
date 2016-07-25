baselines
baseline200: spec
	<version: '2.0.0-baseline'>

	spec for: #common do: [ 
		spec
			blessing: #baseline;
			description: 'Migrated to SmalltalkHub and taking into account new configurations of subcomponents';
			repository: 'http://smalltalkhub.com/mc/Pharo/XMLWriter/main'.
				
		"Does not work without a className: specification
		
		spec project: 'OrderPreservingDictionary' with: [
			spec 
				versionString: #stable; 
				loads: #('Core'); 
				repository: 'http://smalltalkhub.com/mc/Pharo/OrderPreservingDictionary/main'].
		spec package: 'XML-Writer' with: [ spec requires: #('OrderPreservingDictionary') ]."
			
		spec project: 'OrderPreservingDictionary' with: [
			spec 
				className: 'ConfigurationOfOrderPreservingDictionary';
				versionString: #stable; 
				loads: #('Core'); 
				repository: 'http://smalltalkhub.com/mc/Pharo/OrderPreservingDictionary/main'].	
				
		spec package: 'XML-Writer' with: [spec requires: 'OrderPreservingDictionary']. 
			
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer');
			group: 'Tests' with: #() ].
	
	
	
	"I removed it because it loads an old version of the pckage and this is a nonsense"	
	spec for: #squeakCommon do: [ 
		spec package: 'Collections-Support' with: [ 
			spec repository: 'http://ss3.gemstone.com/ss/Pharo20' ] ].
	
	
	
	
	spec for: #gemstone do: [ 
		spec
			package: 'Collections-Support' with: [ 
				spec
					file: 'Collections-Support.g';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ];
			package: 'Collections-Tests-Support' with: [ 
				spec
					file: 'Collections-Tests-Support.g';
					requires: 'Collections-Support';
					repository: 'http://seaside.gemstone.com/ss/XMLSupport' ].
		spec
			group: 'Tests' with: #('Collections-Tests-Support') ]