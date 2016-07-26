versions-1
version11: spec 
        <version: '1.1' imports: #('1.1-baseline') >
        
        spec for: #pharo do: [
                spec blessing: #release.
                spec 
                        package: 'Fuel' with: 'Fuel-MartinDias.84';
                        package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.4';
				package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.4';
				package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.1';
				package: 'FuelBenchmarksMC2' with: 'FuelBenchmarksMC2-MartinDias.1' ].