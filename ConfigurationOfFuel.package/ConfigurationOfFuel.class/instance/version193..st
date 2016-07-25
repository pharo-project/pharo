versions-1
version193: spec
	<version: '1.9.3' imports: #('1.9.3-baseline' )>

	spec for: #common do: [
		spec blessing: #release.
		spec description: '- (feature) the #fuelReplacement selector offers the ability to statically replace an object (e.g. with nil) during analysis. This can lead to significantly smaller graphs and improved speed when serializing large graphs
- (feature) serialize the same graph that was analyzed instead of retrazing the graph during serialization. This prevents changes in the graph from happening between analysis and serialization
- (change) store source when serializing CompiledMethod objects. Needed because Opal does not allow decompilation
- (fix) migrate variables across hierarchy
- (feature) Object>>serializeToFileNamed: shortcut for serializing arbitrary objects
- (fix) better compression for LargeNegativeIntegers (https://pharo.fogbugz.com/f/cases/12052/Fuel-could-store-LargeNegativeInteger-up-to-32bits-magnitude)'.
		spec 
			package: 'Fuel' with: 'Fuel-MartinDias.791';
			package: 'FuelTests' with: 'FuelTests-MaxLeske.371';
			package: 'FuelMetalevel' with: 'FuelMetalevel-MartinDias.73';
			package: 'FuelMetalevelTests' with: 'FuelMetalevelTests-MartinDias.60';
			package: 'FuelDebug' with: 'FuelDebug-MartinDias.12';
			package: 'FuelPreview' with: 'FuelPreview-MartinDias.10';
			package: 'FuelUtilities' with: 'FuelUtilities-MartinDias.1';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.62';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MaxLeske.150';
			
			package: 'FuelCompatibilityBeforePharo12' with: 'FuelCompatibilityBeforePharo12-MaxLeske.12';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MaxLeske.4';
			package: 'FuelCompatibilityBeforePharo14' with: 'FuelCompatibilityBeforePharo14-MartinDias.3';
			package: 'FuelCompatibilityBeforePharo20' with: 'FuelCompatibilityBeforePharo20-MaxLeske.18';
			package: 'FuelCompatibilityBeforePharo30' with: 'FuelCompatibilityBeforePharo30-MaxLeske.5';
			
			package: 'FuelTestsCompatibilityBeforePharo12' with: 'FuelTestsCompatibilityBeforePharo12-MaxLeske.3';
			package: 'FuelTestsCompatibilityBeforePharo20' with: 'FuelTestsCompatibilityBeforePharo20-MaxLeske.5';
			package: 'FuelTestsCompatibilityBeforePharo30' with: 'FuelTestsCompatibilityBeforePharo30-MaxLeske.5';
			
			package: 'FuelCompatibilityBeforeSqueak42' with: 'FuelCompatibilityBeforeSqueak42-MaxLeske.2';
			package: 'FuelCompatibilityBeforeSqueak44' with: 'FuelCompatibilityBeforeSqueak44-MaxLeske.4';
			package: 'FuelCompatibilityBeforeSqueak45' with: 'FuelCompatibilityBeforeSqueak45-MaxLeske.8';
			package: 'FuelCompatibilityBeforeSqueak46' with: 'FuelCompatibilityBeforeSqueak46-MaxLeske.2';
			
			package: 'FuelTestsCompatibilityBeforeSqueak42' with: 'FuelTestsCompatibilityBeforeSqueak42-MaxLeske.4';
			package: 'FuelTestsCompatibilityBeforeSqueak43' with: 'FuelTestsCompatibilityBeforeSqueak43-MaxLeske.1';
			package: 'FuelTestsCompatibilityBeforeSqueak44' with: 'FuelTestsCompatibilityBeforeSqueak44-MaxLeske.5';
			package: 'FuelTestsCompatibilityBeforeSqueak45' with: 'FuelTestsCompatibilityBeforeSqueak45-MaxLeske.11';
			package: 'FuelTestsCompatibilityBeforeSqueak46' with: 'FuelTestsCompatibilityBeforeSqueak46-MaxLeske.3'.

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
			package: 'FuelTools-Debugger' with: 'FuelTools-Debugger-MarcusDenker.9' ].