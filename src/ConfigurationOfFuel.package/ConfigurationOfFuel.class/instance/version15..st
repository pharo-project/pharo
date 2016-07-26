versions-1
version15: spec 
	<version: '1.5' imports: #('1.5-baseline') >

	spec for: #pharo do: [
		spec blessing: #release.
		spec description: '
- Initialize instances after materialization implementing #fuelAfterMaterialization.
- Ignore transient instance variables overriding #fuelIgnoredInstanceVariableNames.
- Limit serialization progress bar to update at most once every 250 milliseconds.
- Traits are serialized and materialized as a regular objects (http://code.google.com/p/fuel/issues/detail?id=59).
- Removed mappers system cache (Eliot).
- Many new tests.
- Benchmarks: compare Fuel against StOMP, SRP, SIXX, Magma, EsAndEm and Fuel with progress bar.
- Benchmarks: new samples, scripts, and CSV exporter.	
- Benchmarks: measure serialization stream size.
- Removed some redundant #fuelAccept:.
- Several aesthetic changes in protocols, categories and comments.
- Now #materialize answers the materialized root.
- Removed special cluster for HashedCollection.Now #rehash is sended via #fuelAfterMaterialization.
- MethodContext serialization does not serialize temps (http://code.google.com/p/fuel/issues/detail?id=61).
- Classes and Traits now serialize its environment (testCreateWithSmalltalkGlobalsEnvironment).
- Fixed serialization of non-octet characters (testCharacter).
- Fixed a bug serializing the system dictionary. FLWellKnownObjectsCluster handles it. (testSmalltalkGlobals).
- Fixed a bug serializing class variables (testClassVariable).
- Fixed a bug when serializing a collection with size greater than 1^16 with repeated elements inside.
- Fixed testTwoCompiledMethodsReferencingSameClassVariable.
- Optimization: instanceIndexes are directly created with the exact size (Henrik).		
		'.
		spec 
			package: 'Fuel' with: 'Fuel-MartinDias.267';
			package: 'FuelTests' with: 'FuelTests-MarianoMartinezPeck.77';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.62';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.14';
			package: 'FuelContainer' with: 'FuelContainer-MartinDias.1';
			package: 'FuelBenchmarksMagma' with: 'FuelBenchmarksMagma-MarianoMartinezPeck.6';
			package: 'FuelBenchmarksSIXX' with: 'FuelBenchmarksSIXX-mariano.3';
			package: 'FuelBenchmarksStomp' with: 'FuelBenchmarksStomp-MarianoMartinezPeck.1';
			package: 'FuelProgressUpdateBenchmarks' with: 'FuelProgressUpdateBenchmarks-MarianoMartinezPeck.4';
			package: 'FuelBenchmarksEsAndEm' with: 'FuelBenchmarksEsAndEm-MarianoMartinezPeck.3';
			package: 'FuelLogo' with: 'FuelLogo-MartinDias.2';
			package: 'SRP' with: 'SRP-MartinDias.12';
			package: 'EsAndEm' with: 'EsAndEm-cwp.15'.
		spec
			project: 'MagmaBase' with: '1.0';
			project: 'SIXX' with: '0.3.6';
			project: 'StOMP' with: '1.8' ].