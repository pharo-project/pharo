versions-1
version17: spec 
	<version: '1.7' imports: #('1.7-baseline') >

	spec for: #pharo do: [
		spec blessing: #release.
		spec description: '
The list of changes includes performance optimizations, design clean-ups, and new features.
- The FLSerializer and FLMaterializer API has changed. They are no longer implementing the algorithms but are a kind of Façade.
- Serialization substitutions: "Store this object instead of me."
- Global sends: "Restore me by sending this selector to this global"
- Versioning the stream: We prefix the stream with a version number that should match when loading.
- Performance optimizations on instances of:
 - Word-like classes. (We thank a lot to Henrik Sperre Johansen for your help!)
 - ByteString and Symbol.
 - Date, Time, Duration and DateAndTime.
 - Point and Rectangle.
 - MethodDictionary. Now materialization is 2000x faster, thanks to its new rehash without become.
- Huge clean-up in Tests package.'.

		spec 
			package: 'Fuel' with: 'Fuel-MartinDias.479';
			package: 'FuelTests' with: 'FuelTests-MartinDias.157';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.88';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.27';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MarianoMartinezPeck.2';
			package: 'FuelPackageLoader' with: 'FuelPackageLoader-MartinDias.43';
			package: 'FuelPackageLoaderTests' with: 'FuelPackageLoaderTests-MartinDias.6';
			package: 'FuelExamples' with: 'FuelExamples-MartinDias.5';
			package: 'FuelContainer' with: 'FuelContainer-MartinDias.3';
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
			project: 'StOMP' with: '1.8'		
		].