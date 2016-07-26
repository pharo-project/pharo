baselines-1
baseline194: spec 
        <version: '1.9.4-baseline'>
        
	spec for: #common do: [
		
		spec
			blessing: #baseline;
			postLoadDoIt: #addPostloadHacks.

		self fuelRepository: spec.    

		spec 
			package: 'Fuel';
			package: 'FuelTests' with: [ spec requires: 'Fuel' ];
			package: 'FuelDebug' with: [ spec requires: 'Fuel' ];
			package: 'FuelUtilities';
			package: 'FuelProgressUpdate' with: [ spec requires: #('Fuel' 'Tests') ];
			package: 'FuelBenchmarks' with: [ spec requires: #('Fuel' 'Tests' 'FuelUtilities' 'FuelMetalevel') ].
			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('Fuel');
			group: 'Tests' with: #('Core' 'FuelTests')
		].

	spec for: #pharo do: [
              
		spec
			package: 'FuelBenchmarksMagma' with: [ spec requires: #('FuelBenchmarks' 'MagmaBase') ];
			package: 'FuelBenchmarksSIXX' with: [ spec requires: #('FuelBenchmarks' 'SIXX') ];
			package: 'FuelBenchmarksStomp' with: [ spec requires: #('FuelBenchmarks' 'StOMP') ];
			package: 'FuelProgressUpdateBenchmarks' with: [ spec requires: #('FuelBenchmarks' 'FuelProgressUpdate') ];
			package: 'FuelBenchmarksSRP' with: [ spec requires: #('FuelBenchmarks' 'SRP') ];
			package: 'FuelBenchmarksEsAndEm' with: [ spec requires: #('FuelBenchmarks' 'EsAndEm') ];
			package: 'FuelPreview' with: [ spec requires: #( 'FuelDebug' 'Roassal')];
			package: 'FuelMetalevel' with: [ spec requires: #( 'Fuel' )];
			package: 'FuelMetalevelTests' with: [ spec requires: #('FuelMetalevel' 'FuelTests' )].
		
		self 
			magma: spec;
			sixx: spec;
			stomp: spec;
			srp: spec;
			esAndEm: spec;
			roassal: spec.

		spec
			group: 'CoreWithExtras' with: #('Core' 'FuelMetalevel' 'FuelProgressUpdate');
			group: 'Tests' with: #('FuelTests' );
			group: 'Benchmarks' with: #('Core' 'FuelBenchmarks');
			group: 'BenchmarksSIXX' with: #('Benchmarks' 'FuelBenchmarksSIXX');
			group: 'BenchmarksAllBinarySerializers' with: #('Benchmarks' 'FuelProgressUpdateBenchmarks' 'FuelBenchmarksStomp' 'FuelBenchmarksMagma' 'FuelBenchmarksSRP' 'FuelBenchmarksEsAndEm');
			group: 'DevelopmentGroup' with: #('CoreWithExtras' 'Tests' 'FuelMetalevelTests' 'Benchmarks' 'FuelProgressUpdateBenchmarks' 'FuelDebug' 'FuelUtilities'). 
	].
		
	spec for: #'pharo2.0.x' do: [
		
		spec	
			package: 'FuelTools-Debugger' with: [ spec requires: #('Fuel' ) ];
			package: 'FuelCommandLineHandler' with: [ spec requires: #('Fuel' ) ];
			package: 'FuelSystem-FileRegistry' with: [ spec requires: #('Fuel' ) ].
												
		spec 
			group: 'kernel' with: #( 'FuelCommandLineHandler' );
			group: 'Core' with: #('FuelCommandLineHandler' 'FuelSystem-FileRegistry' 'FuelTools-Debugger').
	].