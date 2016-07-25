versions-1
version14: spec 
        <version: '1.4' imports: #('1.2-baseline') >
        
        spec for: #pharo do: [
                spec blessing: #release.
		    spec description: '
				- New support for serialization in memory
				- Name refactorings in some hierarchies and protocols.
				- Object cluster big refactoring
				- Hack that avoids several OrderedCollection>>makeRoomAtLast.
				- Other cleanups
			'.
                spec 
                        package: 'Fuel' with: 'Fuel-MartinDias.243';
				package: 'FuelTests' with: 'FuelTests-MarianoMartinezPeck.33';
                        package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.26';
				package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.8';
				package: 'FuelContainer' with: 'FuelContainer-MartinDias.1';
				package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.3';
				package: 'FuelBenchmarksMC2' with: 'FuelBenchmarksMC2-MartinDias.1' ].