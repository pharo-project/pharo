versions-2
version216: spec
	<version: '2.1.6' imports: #('2.1.0-baseline')>

	spec for: #common do: [
		spec blessing: #release.
		spec description: 'Use encodeReferenceToClusterObjectClass: instead of encodeReferenceTo: to reference classes in cluster header (in the same way as FLObjectCluster>>clusterSerializeStepWith: does).'.
		spec
			package: 'Fuel' with: 'Fuel-MartinDias.826';
			package: 'FuelTests' with: 'FuelTests-MartinDias.412';
			package: 'FuelMetalevel' with: 'FuelMetalevel-MaxLeske.83';
			package: 'FuelMetalevelTests' with: 'FuelMetalevelTests-MaxLeske.63';
			package: 'FuelDebug' with: 'FuelDebug-MartinDias.12';
			package: 'FuelPreview' with: 'FuelPreview-MartinDias.13';
			package: 'FuelUtilities' with: 'FuelUtilities-MartinDias.1';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.62';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MaxLeske.150'.
			
		spec 
			project: 'MagmaBase' with: '1.0';
			project: 'SIXX' with: '0.3.6';
			project: 'StOMP' with: '1.8.3';
			project: 'Roassal' with: '1.61'.

		spec 
			package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MartinDias.7';
			package: 'FuelBenchmarksSIXX' with: 'FuelBenchmarksSIXX-MartinDias.4';
			package: 'FuelBenchmarksStomp' with: 'FuelBenchmarksStomp-MartinDias.2';
			package: 'FuelProgressUpdateBenchmarks' with: 'FuelProgressUpdateBenchmarks-MartinDias.12';
			package: 'FuelBenchmarksSRP' with: 'FuelBenchmarksSRP-MartinDias.4';
			package: 'FuelBenchmarksEsAndEm' with: 'FuelBenchmarksEsAndEm-MartinDias.4';
			package: 'SRP' with: 'SRP-MartinDias.12';
			package: 'EsAndEm' with: 'EsAndEm-cwp.15'.

		spec 
			package: 'FuelCommandLineHandler' with: 'FuelCommandLineHandler-MarcusDenker.24';
			package: 'FuelSystem-FileRegistry' with: 'FuelSystem-FileRegistry-EstebanLorenzano.3'; 
			package: 'FuelTools-Debugger' with: 'FuelTools-Debugger-MarcusDenker.9' ]