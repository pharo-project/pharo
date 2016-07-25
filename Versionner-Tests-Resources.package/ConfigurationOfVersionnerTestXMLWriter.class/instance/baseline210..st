baselines
baseline210: spec
	<version: '2.1.0-baseline'>

	spec for: #common do: [ 
		spec
			blessing: #baseline;
			description: 'Adding a test package';
			repository: 'http://smalltalkhub.com/mc/Pharo/XMLWriter/main'.
				
		spec project: 'OrderPreservingDictionary' with: [
			spec 
				className: 'ConfigurationOfOrderPreservingDictionary';
				versionString: #stable; 
				loads: #('Core'); 
				repository: 'http://smalltalkhub.com/mc/Pharo/OrderPreservingDictionary/main'].	
				
		spec package: 'XML-Writer-Core' with: [spec requires: 'OrderPreservingDictionary']. 
		spec package: 'XML-Writer-Tests' with: [spec requires: 'XML-Writer-Core']. 
		
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('XML-Writer-Core');
			group: 'Tests' with: #('XML-Writer-Tests')].
	
	
	"Can only work for Pharo1.4 and 2.0.
	To be changed when pharo will be managed on SmalltalkHub"
		
	spec for: #squeakCommon do: [ 
		spec package: 'Collections-Support' with: [ 
			spec repository: 'http://ss3.gemstone.com/ss/Pharo20' ] ].
	
	
	"I did not touch this part because Ido not know it. On Pharo XMLWriter has its own repository"
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