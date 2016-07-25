versions
version12: spec 
	<version: '1.2' imports: #('1.0-baseline')>
	
	spec for: #common do: [
		spec blessing: #development.
		spec description: 'Add in doits for Example-Core'.
		spec author: 'dkh'.
		spec timestamp: '10/12/2009 09:26'.
		spec 
			package: 'Example-Core' with: [
				spec 
					file: 'Example-Core-anon.16';
					preLoadDoIt: #preloadForCore;
					postLoadDoIt: #postloadForCore:package: ];
			package: 'Example-Tests' with: 'Example-Tests-anon.5';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].
