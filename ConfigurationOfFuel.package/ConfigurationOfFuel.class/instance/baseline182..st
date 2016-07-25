baselines-1
baseline182: spec 
        <version: '1.8.2-baseline'>
        
	spec for: #squeakCommon do: [
		
		spec blessing: #baseline.
		self fuelRepository: spec.    

		spec 
			package: 'Fuel';
			package: 'FuelTests' with: [ spec requires: 'Fuel' ];
			package: 'FuelDebug' with: [ spec requires: 'Fuel' ];
			package: 'FuelUtilities';
			package: 'FuelProgressUpdate' with: [ spec requires: 'Fuel' ];
			package: 'FuelBenchmarks' with: [ spec requires: #('Fuel' 'FuelTests' 'FuelUtilities') ];
			package: 'FuelCompression'  with: [ spec requires: #('Fuel' 'FuelTests' ) ].
							
		spec
			package: 'FuelCompatibilityBeforeSqueak42' with: [ spec requires: #( 'FuelCompatibilityBeforeSqueak44' ) ];	
			package: 'FuelCompatibilityBeforeSqueak44' with: [ spec requires: #( 'FuelCompatibilityBeforeSqueak45' ) ];	
			package: 'FuelCompatibilityBeforeSqueak45' with: [ spec requires: #( 'Fuel' ) ];
			
			package: 'FuelTestsCompatibilityBeforeSqueak42' with: [ spec requires: #( 'FuelTestsCompatibilityBeforeSqueak44' ) ];
			package: 'FuelTestsCompatibilityBeforeSqueak44' with: [ spec requires: #( 'FuelTestsCompatibilityBeforeSqueak45' ) ];
			package: 'FuelTestsCompatibilityBeforeSqueak45' with: [ spec requires: #( 'FuelTests' ) ].
			
		spec
			package: 'FuelCompatibilityBeforePharo12' with: [ spec requires: #( 'FuelCompatibilityBeforePharo13' ) ];
			package: 'FuelCompatibilityBeforePharo13' with: [ spec requires: #( 'FuelCompatibilityBeforePharo14' ) ];
			package: 'FuelCompatibilityBeforePharo14' with: [ spec requires: #( 'FuelCompatibilityBeforePharo20' ) ];
			package: 'FuelCompatibilityBeforePharo20' with: [ spec requires: #( 'FuelCompatibilityBeforePharo21' ) ];
			package: 'FuelCompatibilityBeforePharo21' with: [ spec requires: #( 'Fuel' ) ];
			
			package: 'FuelTestsCompatibilityBeforePharo20' with: [ spec requires: #( 
				'FuelCompatibilityBeforePharo21' 
				'FuelTestsCompatibilityBeforePharo21' ) ];
			package: 'FuelTestsCompatibilityBeforePharo21' with: [ spec requires: #( 'FuelTests' ) ].
			
		spec
			group: 'default' with: #('Core' 'Tests');
			group: 'Core' with: #('Fuel');
			group: 'CoreWithExtras' with: #('Core' 'FuelProgressUpdate' 'FuelCompression');
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
			package: 'FuelPackageLoader' with: [ spec requires: #('Fuel' 'FuelMetalevel' ) ];	
			package: 'FuelPackageLoaderMetacello' with: [ spec requires: #( 'FuelPackageLoader' ) ];	
			package: 'FuelPackageLoaderTests' with: [ spec requires: #('FuelPackageLoader' 'FuelTests' )];
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
				project: 'FileSystem' with: [
				spec
					className: 'ConfigurationOfFilesystem';
					loads: #('default');
					file: 'ConfigurationOfFilesystem';
					repository: 'http://www.squeaksource.com/fs' ].

			spec
				package: 'SRP' with: [ spec repository: 'http://www.squeaksource.com/SRP' ].

			spec
				package: 'EsAndEm' with: [ spec repository: 'http://source.wiresong.ca/mc' ].

			spec
				group: 'CoreWithExtras' with: #('Core'  'FuelMetalevel' 'FuelProgressUpdate' 'FuelCompression');
				group: 'Tests' with: #('FuelTests' );
				group: 'PackageLoader' with: #('FuelPackageLoader' 'FuelPackageLoaderMetacello');
				group: 'PackageLoaderWithTests' with: #('PackageLoader' 'FuelPackageLoaderTests');
				group: 'BenchmarksSIXX' with: #('Benchmarks' 'FuelBenchmarksSIXX');
				group: 'BenchmarksAllBinarySerializers' with: #('Benchmarks' 'FuelProgressUpdateBenchmarks' 'FuelBenchmarksStomp' 'FuelBenchmarksMagma' 'FuelBenchmarksSRP' 'FuelBenchmarksEsAndEm');
				group: 'DevelopmentGroup' with: #('CoreWithExtras' 'Tests' 'FuelMetalevelTests' 'Benchmarks' 'FuelProgressUpdateBenchmarks' PackageLoader 'PackageLoaderWithTests' 'FuelDebug' 'FuelUtilities').
				
	].
		
	
	spec for: #'pharo2.0.x' do: [
		
		spec	
			package: 'FuelFileSystem';
			package: 'FuelFileSystemTests' with: [ spec requires: #('FuelTests')];
			package: 'FuelCommandLineHandler'.
								
		spec group: 'DevelopmentGroup' with: #('FuelFileSystem' 'FuelFileSystemTests' 'FuelCommandLineHandler').
		
		spec group: 'Core' with: #('FuelCompatibilityBeforePharo21').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo21').
		
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
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforePharo20').
		
	].

	spec for: #'squeak4.1.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak42').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak42').
			
	].

	spec for: #'squeak4.2.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak44').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak44').
			
	].

	spec for: #'squeak4.3.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak44').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak44').
			
	].

	spec for: #'squeak4.4.x' do: [
			
		spec group: 'Core' with: #('FuelCompatibilityBeforeSqueak45').
		spec group: 'Tests' with: #('FuelTestsCompatibilityBeforeSqueak45').
			
	].