versions-1
version13: spec 
        <version: '1.3' imports: #('1.2-baseline') >
        
        spec for: #pharo do: [
                spec blessing: #release.
		    spec description: '
					- Class serialization without ClassBuilder.
					- MethodContext and BlockClosure serialization.
					- Some fixes on configuration itself
				'.
                spec 
                        package: 'Fuel' with: 'Fuel-MarianoMartinezPeck.225';
				package: 'FuelTests' with: 'FuelTests-MarianoMartinezPeck.21';
                        package: 'FuelBenchmarks' with: 'FuelBenchmarks-MarianoMartinezPeck.20';
				package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.6';
				package: 'FuelContainer' with: 'FuelContainer-MartinDias.1';
				package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.3';
				package: 'FuelBenchmarksMC2' with: 'FuelBenchmarksMC2-MartinDias.1' ].