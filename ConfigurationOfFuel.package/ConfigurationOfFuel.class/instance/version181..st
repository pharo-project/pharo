versions-1
version181: spec
	<version: '1.8.1' imports: #('1.8.1-baseline' )>

	spec for: #'squeakCommon' do: [
		spec blessing: #release.
		spec description: '
- same as 1.8
- fix: serialization / materialization of Date objects.
- do not use Pharo compatibility packages for Squeak
- use individual compatibility packages for Squeak
- new compatibility fixes
- supported images: 
	Pharo 1.1.1, 1.1.2, 1.2, 1.3, 1.4, 2.0
	Squeak 4.1, 4.2, 4.3, 4.4'.

		spec 
			package: 'Fuel' with: 'Fuel-MaxLeske.686';
			package: 'FuelTests' with: 'FuelTests-MartinDias.267';
			package: 'FuelDebug' with: 'FuelDebug-MartinDias.8';
			package: 'FuelUtilities' with: 'FuelUtilities-MartinDias.1';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.54';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.140';
			package: 'FuelCompression' with: 'FuelCompression-MartinDias.9';
			
			package: 'FuelCompatibilityBeforeSqueak42' with: 'FuelCompatibilityBeforeSqueak42-MaxLeske.1';
			package: 'FuelCompatibilityBeforeSqueak44' with: 'FuelCompatibilityBeforeSqueak44-MaxLeske.4';
			package: 'FuelCompatibilityBeforeSqueak45' with: 'FuelCompatibilityBeforeSqueak45-MaxLeske.3';
			
			package: 'FuelTestsCompatibilityBeforeSqueak42' with: 'FuelTestsCompatibilityBeforeSqueak42-MaxLeske.2';
			package: 'FuelTestsCompatibilityBeforeSqueak44' with: 'FuelTestsCompatibilityBeforeSqueak44-MaxLeske.5';
			package: 'FuelTestsCompatibilityBeforeSqueak45' with: 'FuelTestsCompatibilityBeforeSqueak45-MaxLeske.6';
			
			package: 'FuelCompatibilityBeforePharo12' with: 'FuelCompatibilityBeforePharo12-MartinDias.8';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MarianoMartinezPeck.3';
			package: 'FuelCompatibilityBeforePharo14' with: 'FuelCompatibilityBeforePharo14-MartinDias.3';
			package: 'FuelCompatibilityBeforePharo20' with: 'FuelCompatibilityBeforePharo20-MaxLeske.12';
			package: 'FuelCompatibilityBeforePharo21' with: 'FuelCompatibilityBeforePharo21-MaxLeske.3';
			
			package: 'FuelTestsCompatibilityBeforePharo21' with: 'FuelTestsCompatibilityBeforePharo21-MaxLeske.5' ].

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