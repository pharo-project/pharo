versions-1
version12: spec 
        <version: '1.2' imports: #('1.2-baseline') >
        
        spec for: #pharo do: [
                spec blessing: #release.
                spec 
                        package: 'Fuel' with: 'Fuel-MartinDias.193';
				package: 'FuelTests' with: 'FuelTests-MartinDias.2';
                        package: 'FuelBenchmarks' with: 'FuelBenchmarks-MarianoMartinezPeck.9';
				package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.6';
				package: 'FuelContainer' with: 'FuelContainer-MartinDias.1';
				package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.3';
				package: 'FuelBenchmarksMC2' with: 'FuelBenchmarksMC2-MartinDias.1' ].