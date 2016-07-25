baselines-1
baseline16: spec 
        <version: '1.6-baseline'>
        
        spec for: #pharo do: [
                spec blessing: #baseline.
                self fuelRepository: spec.    

                spec 
                        package: 'Fuel';
				package: 'FuelTests' with: [ spec requires: 'Fuel' ];
                        package: 'FuelProgressUpdate' with: [ spec requires: 'Fuel' ];
                        package: 'FuelContainer' with: [ spec requires: 'Fuel' ];
                        package: 'FuelBenchmarks' with: [ spec requires: #('Fuel' 'FuelTests') ];
                        package: 'FuelBenchmarksMagma' with: [ spec requires: #('FuelBenchmarks' 'MagmaBase') ];
                        package: 'FuelBenchmarksSIXX' with: [ spec requires: #('FuelBenchmarks' 'SIXX') ];
                        package: 'FuelBenchmarksStomp' with: [ spec requires: #('FuelBenchmarks' 'StOMP') ];
				package: 'FuelProgressUpdateBenchmarks' with: [ spec requires: #('FuelBenchmarks' 'FuelProgressUpdate') ];
                        package: 'FuelBenchmarksSRP' with: [ spec requires: #('FuelBenchmarks' 'SRP') ];
                        package: 'FuelBenchmarksEsAndEm' with: [ spec requires: #('FuelBenchmarks' 'EsAndEm') ];
                        package: 'FuelLogo';
				package: 'FuelExamples' with: [ spec requires: 'FuelProgressUpdate'];
				package: 'FuelPackageLoader' with: [ spec requires: 'Fuel' ];	
				package: 'FuelPackageLoaderTests' with: [ spec requires: #('FuelPackageLoader' 'FuelTests' ) ].	

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
				package: 'SRP' with: [ spec repository: 'http://www.squeaksource.com/SRP' ].

			spec
				package: 'EsAndEm' with: [ spec repository: 'http://source.wiresong.ca/mc' ].

			spec
				group: 'default' with: #('Core' 'Tests' 'Benchmarks' 'Examples');
				group: 'Core' with: #('Fuel');
				group: 'CoreWithExtras' with: #('Fuel' 'FuelProgressUpdate' 'FuelContainer');
				group: 'Tests' with: #('FuelTests');
				group: 'Examples' with: #('FuelExamples');
				group: 'PackageLoader' with: #('FuelPackageLoader');
				group: 'PackageLoaderWithTests' with: #('FuelPackageLoader' 'FuelPackageLoaderTests');
				group: 'Benchmarks' with: #('FuelBenchmarks');
				group: 'BenchmarksSIXX' with: #('Benchmarks' 'FuelBenchmarksSIXX');
				group: 'BenchmarksAllBinarySerializers' with: #('Benchmarks' 'FuelProgressUpdateBenchmarks' 'FuelBenchmarksStomp' 'FuelBenchmarksMagma' 'FuelBenchmarksSRP' 'FuelBenchmarksEsAndEm') ].