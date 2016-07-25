baselines-1
baseline19: spec 
        <version: '1.9-baseline'>
        
	spec for: #common do: [
		
		spec blessing: #baseline.
		self fuelRepository: spec.    

		spec 
			package: 'Fuel';
			package: 'FuelTests' with: [ spec requires: 'Fuel' ];
			package: 'FuelDebug' with: [ spec requires: 'Fuel' ];
			package: 'FuelUtilities';
			package: 'FuelProgressUpdate' with: [ spec requires: 'Fuel' ];
			package: 'FuelBenchmarks' with: [ spec requires: #('Fuel' 'FuelTests' 'FuelUtilities' 'FuelMetalevel') ].
							
		spec
			package: 'FuelCompatibilityBeforePharo12' with: [ spec requires: #( 'FuelCompatibilityBeforePharo13' ) ];
			package: 'FuelCompatibilityBeforePharo13' with: [ spec requires: #( 'FuelCompatibilityBeforePharo14' ) ];
			package: 'FuelCompatibilityBeforePharo14' with: [ spec requires: #( 'FuelCompatibilityBeforePharo20' ) ];
			package: 'FuelCompatibilityBeforePharo20' with: [ spec requires: #( 'Fuel' ) ].
			
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
			group: 'Tests' with: #('FuelTests');
			group: 'Benchmarks' with: #('FuelBenchmarks').
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
		
                spec
				project: 'MagmaBase' with: [
				spec
					className: 'ConfigurationOfMaBase';
					loads: #('default');
					file: 'ConfigurationOfMaBase';
					repository: 'http://www.squeaksource.com/MetacelloRepository' ].

                spec
				project: 'SIXX' with: [
				spec
					className: 'ConfigurationOfSIXX';
					loads: #('default');
					file: 'ConfigurationOfSIXX';
					repository: 'http://www.squeaksource.com/MetacelloRepository' ].
				
		     spec
				project: 'StOMP' with: [
				spec
					className: 'ConfigurationOfStOMP';
					loads: #('default');
					file: 'ConfigurationOfStOMP';
					repository: 'http://www.squeaksource.com/MetacelloRepository' ].

                spec
				project: 'Roassal' with: [
				spec
					className: 'ConfigurationOfRoassal';
					loads: #('default');
					file: 'ConfigurationOfRoassal';
					repository: 'http://www.squeaksource.com/Roassal' ].

			spec
				package: 'SRP' with: [ spec repository: 'http://www.squeaksource.com/SRP' ].

			spec
				package: 'EsAndEm' with: [ spec repository: 'http://source.wiresong.ca/mc' ].

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
												
		spec group: 'kernel' with: #( 'FuelCommandLineHandler' ).
		spec group: 'Core' with: #('FuelCommandLineHandler' 'FuelSystem-FileRegistry' 'FuelTools-Debugger').	
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo30').
	].
	
	
	spec for: #'pharo1.4.x' do: [
		
		spec group: 'Core' with: #('FuelCompatibilityBeforePharo20').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].
	
	
	spec for: #'pharo1.3.x' do: [
		
		spec group: 'Core' with: #('FuelCompatibilityBeforePharo14').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].
	
	
	spec for: #'pharo1.2.x' do: [
		
		spec group: 'Core' with: #('FuelCompatibilityBeforePharo13').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].


	spec for: #'pharo1.1.x' do: [
	
		spec 
			package: 'Fuel' with: [ spec preLoadDoIt: #preLoadInPharo11].
		
		spec group: 'Core' with: #('FuelCompatibilityBeforePharo12').
		
	].


	spec for: #'squeak4.1.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak42').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak42').
			
	].


	spec for: #'squeak4.2.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak44').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak43').
			
	].


	spec for: #'squeak4.3.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak44').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak44').
			
	].


	spec for: #'squeak4.4.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak45').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak45').
			
	].