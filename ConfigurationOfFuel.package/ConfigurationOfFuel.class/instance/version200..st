versions-2
version200: spec
	<version: '2.0.0' imports: #('2.0.0-baseline' )>

	spec for: #common do: [
		spec blessing: #release.
		spec description: 'Prepared for Spur Object Format'.
		spec 
			package: 'Fuel' with: 'Fuel-EstebanLorenzano.819';
			package: 'FuelTests' with: 'FuelTests-EstebanLorenzano.395';
			package: 'FuelMetalevel' with: 'FuelMetalevel-MartinDias.75';
			package: 'FuelMetalevelTests' with: 'FuelMetalevelTests-MaxLeske.61';
			package: 'FuelDebug' with: 'FuelDebug-MartinDias.12';
			package: 'FuelPreview' with: 'FuelPreview-MartinDias.13';
			package: 'FuelUtilities' with: 'FuelUtilities-MartinDias.1';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.62';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MaxLeske.150';
			
			package: 'FuelCompatibilityBeforePharo12' with: 'FuelCompatibilityBeforePharo12-MaxLeske.12';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MaxLeske.4';
			package: 'FuelCompatibilityBeforePharo14' with: 'FuelCompatibilityBeforePharo14-MartinDias.3';
			package: 'FuelCompatibilityBeforePharo20' with: 'FuelCompatibilityBeforePharo20-MaxLeske.18';
			package: 'FuelCompatibilityBeforePharo30' with: 'FuelCompatibilityBeforePharo30-MaxLeske.5';
			package: 'FuelCompatibilityBeforePharo40' with: 'FuelCompatibilityBeforePharo40-MaxLeske.1';
			
			package: 'FuelTestsCompatibilityBeforePharo12' with: 'FuelTestsCompatibilityBeforePharo12-MaxLeske.3';
			package: 'FuelTestsCompatibilityBeforePharo20' with: 'FuelTestsCompatibilityBeforePharo20-MaxLeske.5';
			package: 'FuelTestsCompatibilityBeforePharo30' with: 'FuelTestsCompatibilityBeforePharo30-MaxLeske.6';
			
			package: 'FuelCompatibilityBeforeSqueak42' with: 'FuelCompatibilityBeforeSqueak42-MaxLeske.2';
			package: 'FuelCompatibilityBeforeSqueak44' with: 'FuelCompatibilityBeforeSqueak44-MaxLeske.4';
			package: 'FuelCompatibilityBeforeSqueak45' with: 'FuelCompatibilityBeforeSqueak45-MaxLeske.8';
			package: 'FuelCompatibilityBeforeSqueak46' with: 'FuelCompatibilityBeforeSqueak46-MaxLeske.3';
			package: 'FuelCompatibilityBeforeSqueak47' with: 'FuelCompatibilityBeforeSqueak47-MaxLeske.2';
			
			package: 'FuelTestsCompatibilityBeforeSqueak42' with: 'FuelTestsCompatibilityBeforeSqueak42-MaxLeske.4';
			package: 'FuelTestsCompatibilityBeforeSqueak43' with: 'FuelTestsCompatibilityBeforeSqueak43-MaxLeske.1';
			package: 'FuelTestsCompatibilityBeforeSqueak44' with: 'FuelTestsCompatibilityBeforeSqueak44-MaxLeske.5';
			package: 'FuelTestsCompatibilityBeforeSqueak45' with: 'FuelTestsCompatibilityBeforeSqueak45-MaxLeske.11';
			package: 'FuelTestsCompatibilityBeforeSqueak46' with: 'FuelTestsCompatibilityBeforeSqueak46-MaxLeske.4';
			package: 'FuelTestsCompatibilityBeforeSqueak47' with: 'FuelTestsCompatibilityBeforeSqueak47-MaxLeske.4'.

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