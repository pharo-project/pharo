versions-1
version10: spec 
        <version: '1.0' imports: #('1.0-baseline') >
        
        spec for: #pharo do: [
                spec blessing: #release.
                spec 
                        package: 'Fuel' with: 'Fuel-MartinDias.84';
                        package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.4';
				package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.1';
				package: 'FuelBenchmarksMC2' with: 'FuelBenchmarksMC2-MartinDias.1' ].