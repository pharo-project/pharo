versions-1
version18ForMarea: spec 
	<version: '1.8-forMarea' imports: #('1.8-baseline') >

	spec for: #pharo do: [
		spec blessing: #beta.
		spec description: ' just a fixed version for marea paper '.

		spec 
			package: 'Fuel' with: 'Fuel-MarianoMartinezPeck.618';
			package: 'FuelTests' with: 'FuelTests-MarianoMartinezPeck.214';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.105';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MarianoMartinezPeck.39';
			package: 'FuelCompatibilityBeforePharo12' with: 'FuelCompatibilityBeforePharo12-MarianoMartinezPeck.6';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MarianoMartinezPeck.2';
			package: 'FuelCompatibilityBeforePharo14' with: 'FuelCompatibilityBeforePharo14-MarianoMartinezPeck.1';
			package: 'FuelCompatibilityForSqueak' with: 'FuelCompatibilityForSqueak-MMP.4';
			package: 'FuelPackageLoader' with: 'FuelPackageLoader-MarianoMartinezPeck.53';
			package: 'FuelPackageLoaderTests' with: 'FuelPackageLoaderTests-MarianoMartinezPeck.7';
			package: 'FuelExamples' with: 'FuelExamples-MarianoMartinezPeck.8';
			package: 'FuelContainer' with: 'FuelContainer-MartinDias.3';
			package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MarianoMartinezPeck.6';
			package: 'FuelBenchmarksSIXX' with: 'FuelBenchmarksSIXX-mariano.3';
			package: 'FuelBenchmarksStomp' with: 'FuelBenchmarksStomp-MarianoMartinezPeck.1';
			package: 'FuelProgressUpdateBenchmarks' with: 'FuelProgressUpdateBenchmarks-MartinDias.7';
			package: 'FuelBenchmarksEsAndEm' with: 'FuelBenchmarksEsAndEm-MarianoMartinezPeck.3';
			package: 'FuelLogo' with: 'FuelLogo-MartinDias.2';
			package: 'FuelCompression' with: 'FuelCompression-MarianoMartinezPeck.7';
			package: 'FuelMetalevel' with: 'FuelMetalevel-MarianoMartinezPeck.28';
			package: 'FuelMetalevelTests' with: 'FuelMetalevelTests-MarianoMartinezPeck.15';
			package: 'FuelPresentations' with: 'FuelPresentations-MartinDias.6';
			package: 'FuelFileSystem' with: 'FuelFileSystem-MarianoMartinezPeck.3';
			package: 'FuelFileSystemTests' with: 'FuelFileSystemTests-MarianoMartinezPeck.1';
			package: 'FuelPreview' with: 'FuelPreview-MartinDias.7';
			package: 'SRP' with: 'SRP-MartinDias.12';
			package: 'EsAndEm' with: 'EsAndEm-cwp.15'.
		spec
			project: 'MagmaBase' with: '1.0';
			project: 'SIXX' with: '0.3.6';
			project: 'StOMP' with: '1.8'		


		].