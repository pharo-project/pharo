baselines-1
baseline192: spec 
        <version: '1.9.2-baseline'>
        
	spec for: #common do: [
		
		spec blessing: #baseline.
		self fuelRepository: spec.    

		spec 
			package: 'Fuel';
			package: 'FuelTests' with: [ spec requires: 'Fuel' ];
			package: 'FuelDebug' with: [ spec requires: 'Fuel' ];
			package: 'FuelUtilities';
			package: 'FuelProgressUpdate' with: [ spec requires: #('Fuel' 'Tests') ];
			package: 'FuelBenchmarks' with: [ spec requires: #('Fuel' 'Tests' 'FuelUtilities' 'FuelMetalevel') ].
							
		spec
			package: 'FuelCompatibilityBeforePharo12' with: [ spec requires: #( 'FuelCompatibilityBeforePharo13' ) ];
			package: 'FuelCompatibilityBeforePharo13' with: [ spec requires: #( 'FuelCompatibilityBeforePharo14' ) ];
			package: 'FuelCompatibilityBeforePharo14' with: [ spec requires: #( 'FuelCompatibilityBeforePharo20' ) ];
			package: 'FuelCompatibilityBeforePharo20' with: [ spec requires: #( 'FuelCompatibilityBeforePharo30' ) ];
			package: 'FuelCompatibilityBeforePharo30' with: [ spec requires: #( 'Fuel' ) ].
			
		spec
			package: 'FuelCompatibilityBeforeSqueak42' with: [ spec requires: #( 'FuelCompatibilityBeforeSqueak44' ) ];	
			package: 'FuelCompatibilityBeforeSqueak44' with: [ spec requires: #( 'FuelCompatibilityBeforeSqueak45' ) ];	
			package: 'FuelCompatibilityBeforeSqueak45' with: [ spec requires: #( 'Fuel' ) ].

		spec
			package: 'FuelTestsCompatibilityBeforePharo12' with: [ spec requires: #( 'FuelTestsCompatibilityBeforePharo20' ) ];
			package: 'FuelTestsCompatibilityBeforePharo20' with: [ spec requires: #( 'FuelTestsCompatibilityBeforePharo30' ) ];
			package: 'FuelTestsCompatibilityBeforePharo30' with: [ spec requires: #( 'FuelTests' ) ].

		spec
			package: 'FuelTestsCompatibilityBeforeSqueak42' with: [ spec requires: #( 'FuelTestsCompatibilityBeforeSqueak43' ) ];
			package: 'FuelTestsCompatibilityBeforeSqueak43' with: [ spec requires: #( 'FuelTestsCompatibilityBeforeSqueak44' ) ];
			package: 'FuelTestsCompatibilityBeforeSqueak44' with: [ spec requires: #( 'FuelTestsCompatibilityBeforeSqueak45' ) ];
			package: 'FuelTestsCompatibilityBeforeSqueak45' with: [ spec requires: #( 'FuelTests' ) ].
			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('Fuel');
			group: 'CoreWithExtras' with: #('Core' 'FuelProgressUpdate');
			group: 'Tests' with: #('Core' 'FuelTests');
			group: 'Benchmarks' with: #('Core' 'FuelBenchmarks').
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
			group: 'CoreWithExtras' with: #('Core'  'FuelMetalevel' 'FuelProgressUpdate');
			group: 'Tests' with: #('FuelTests' );
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
			group: 'Core' with: #('FuelCompatibilityBeforePharo30' 'FuelCommandLineHandler' 'FuelSystem-FileRegistry' 'FuelTools-Debugger');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo30').
	].
	
	
	spec for: #'pharo1.4.x' do: [
		
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforePharo20');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].
	
	
	spec for: #'pharo1.3.x' do: [
		
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforePharo14');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].
	
	
	spec for: #'pharo1.2.x' do: [
		
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforePharo13');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].


	spec for: #'pharo1.1.x' do: [
	
		spec 
			package: 'Fuel' with: [ spec preLoadDoIt: #preLoadInPharo11].
		
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforePharo12');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo12').
		
	].


	spec for: #'squeak4.1.x' do: [
			
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforeSqueak42');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak42').
			
	].


	spec for: #'squeak4.2.x' do: [
			
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforeSqueak44');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak43').
			
	].


	spec for: #'squeak4.3.x' do: [
			
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforeSqueak44');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak44').
			
	].


	spec for: #'squeak4.4.x' do: [
			
		spec 
			group: 'Core' with: #('FuelCompatibilityBeforeSqueak45');
			group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak45').
			
	].