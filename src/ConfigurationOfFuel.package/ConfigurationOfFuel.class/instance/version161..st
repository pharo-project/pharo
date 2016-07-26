versions-1
version161: spec 
	<version: '1.6.1' imports: #('1.6-baseline') >

	spec for: #pharo do: [
		spec blessing: #development.
		spec description: ' . '.
		spec 
			package: 'Fuel' with: 'Fuel-MarianoMartinezPeck.444';
			package: 'FuelTests' with: 'FuelTests-MartinDias.118';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.77';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.24';
			package: 'FuelPackageLoader' with: 'FuelPackageLoader-MartinDias.42';
			package: 'FuelPackageLoaderTests' with: 'FuelPackageLoaderTests-MartinDias.3';
			package: 'FuelContainer' with: 'FuelContainer-MarianoMartinezPeck.2';
			package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MarianoMartinezPeck.6';
			package: 'FuelBenchmarksSIXX' with: 'FuelBenchmarksSIXX-mariano.3';
			package: 'FuelBenchmarksStomp' with: 'FuelBenchmarksStomp-MarianoMartinezPeck.1';
			package: 'FuelProgressUpdateBenchmarks' with: 'FuelProgressUpdateBenchmarks-MartinDias.7';
			package: 'FuelBenchmarksEsAndEm' with: 'FuelBenchmarksEsAndEm-MarianoMartinezPeck.3';
			package: 'FuelLogo' with: 'FuelLogo-MartinDias.2';
			package: 'SRP' with: 'SRP-MartinDias.12';
			package: 'EsAndEm' with: 'EsAndEm-cwp.15'.
		spec
			project: 'MagmaBase' with: '1.0';
			project: 'SIXX' with: '0.3.6';
			project: 'StOMP' with: '1.8' ].