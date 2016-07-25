baselines-2
baseline210: spec 
	<version: '2.1.0-baseline'>
        
	spec for: #common do: [
		
		spec
			blessing: #baseline;
			postLoadDoIt: #addPostloadHacks.

		self fuelRepository: spec.    
		
		self smark: spec.

		spec
			package: 'Fuel';
			package: 'FuelTests' with: [ spec requires: 'Fuel' ];
			package: 'FuelDebug' with: [ spec requires: 'Fuel' ];
			package: 'FuelProgressUpdate' with: [ spec requires: #('Fuel' 'Tests') ];
			package: 'FuelBenchmarks' with: [ spec requires: #('Fuel' 'Tests' 'SMark-Jenkins') ].
					
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('Fuel');
			group: 'CoreWithExtras' with: #('Core' 'FuelProgressUpdate');
			group: 'Tests' with: #('Core' 'FuelTests');
			group: 'Benchmarks' with: #('Core' 'FuelBenchmarks').
	].

	self groupsForBaseline200PharoCommon: spec