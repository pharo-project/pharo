versions-1
version18: spec
	<version: '1.8' imports: #('1.8-baseline' )>

	spec for: #'squeakCommon' do: [
		spec blessing: #release.
		spec description: '
- Improved API for customizing how graph is traced.
- FuelMetalevel: serialization of stuff like classes and compiled methods  moved to an optional package.
- Customize objects to be treated as globals.
- Migrations: declare at materialization time class and variables renames.
- Weak references properly managed. Thanks to Juan Vuletich and Levente.
- Thanks Pavel for helping us improving FuelPackageLoader with wonderful ideas and bug reports.
- Serialization speed up on large graphs by using specialized collections. Thanks Levente.
- Encoder and Decoder: new reifications that clarify the design. Thanks Colin Putney.
- Optimized serialization of clean BlockClosures, which do not need the whole stack of contexts. Thanks Juan Vuletich and Eliot Miranda.
- Added a clear error hierarchy.
- Debug facilities, including graph visualization using Roassal. Thanks Alexandre and Doru.
- Many more new tests. Including those from extension packages, we have almost 600 tests.
- Repository moved from squeaksource to ss3.'.

		spec 
			package: 'Fuel' with: 'Fuel-MartinDias.685';
			package: 'FuelTests' with: 'FuelTests-MartinDias.267';
			package: 'FuelDebug' with: 'FuelDebug-MartinDias.8';
			package: 'FuelUtilities' with: 'FuelUtilities-MartinDias.1';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.54';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.140';
			package: 'FuelCompression' with: 'FuelCompression-MartinDias.9';
			package: 'FuelCompatibilityForSqueak' with: 'FuelCompatibilityForSqueak-MMP.4';
			package: 'FuelCompatibilityBeforePharo12' with: 'FuelCompatibilityBeforePharo12-MartinDias.8';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MarianoMartinezPeck.3';
			package: 'FuelCompatibilityBeforePharo14' with: 'FuelCompatibilityBeforePharo14-MartinDias.3';
			package: 'FuelCompatibilityBeforePharo20' with: 'FuelCompatibilityBeforePharo20-MartinDias.4'. ].

	spec for: #'pharo' do: [
		spec 
			project: 'MagmaBase' with: '1.0';
			project: 'SIXX' with: '0.3.6';
			project: 'StOMP' with: '1.8.3';
			project: 'Roassal' with: '1.0';
			project: 'FileSystem' with: '2.0.4'.
		spec 
			package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.7';
			package: 'FuelBenchmarksSIXX' with: 'FuelBenchmarksSIXX-MartinDias.4';
			package: 'FuelBenchmarksStomp' with: 'FuelBenchmarksStomp-MartinDias.2';
			package: 'FuelProgressUpdateBenchmarks' with: 'FuelProgressUpdateBenchmarks-MartinDias.12';
			package: 'FuelBenchmarksSRP' with: 'FuelBenchmarksSRP-MartinDias.4';
			package: 'FuelBenchmarksEsAndEm' with: 'FuelBenchmarksEsAndEm-MartinDias.4';
			package: 'FuelPackageLoader' with: 'FuelPackageLoader-MarianoMartinezPeck.105';
			package: 'FuelPackageLoaderMetacello' with: 'FuelPackageLoaderMetacello-MarianoMartinezPeck.3';
			package: 'FuelPackageLoaderTests' with: 'FuelPackageLoaderTests-MarianoMartinezPeck.32';
			package: 'FuelPreview' with: 'FuelPreview-MartinDias.10';
			package: 'FuelMetalevel' with: 'FuelMetalevel-MarianoMartinezPeck.50';
			package: 'FuelMetalevelTests' with: 'FuelMetalevelTests-MartinDias.43';
			package: 'SRP' with: 'SRP-MartinDias.12';
			package: 'EsAndEm' with: 'EsAndEm-cwp.15'. ].

	spec for: #'pharo2.0.x' do: [
		spec 
			package: 'FuelFileSystem' with: 'FuelFileSystem-PavelKrivanek.4';
			package: 'FuelFileSystemTests' with: 'FuelFileSystemTests-MarianoMartinezPeck.2';
			package: 'FuelCommandLineHandler' with: 'FuelCommandLineHandler-MarianoMartinezPeck.3'. ].