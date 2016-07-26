versions-1
version16: spec 
	<version: '1.6' imports: #('1.5-baseline') >

	spec for: #pharo do: [
		spec blessing: #release.
		spec description: '
This version does not add any new concrete functionality but has two main achievements: a performance improvement and a design clean up.

Many changes included in this release are part of a design clean up. They are a necessary step forward to new functionalities that are coming soon. They make code more understandable. They enable simpler extension. Happily they also helped to improve serialization performance by reducing unnecessary interactions. And is specially important a bottleneck in graph analysis stage we have attacked, obtaining great results.

Details:
 - Removed explicit references to cluster classes scattered around the system. For example, in previous versions Float>>fuelCluster has an explicit reference to FLFloatCluster. This version replaces this by a double dispatch interaction.
 - Clusters: Better reification. Removed annoying IDs. Removed Singleton instance creation machanism, now mappers manage uniqueness in a better way. 
 - Mappers: They were simplified. New mapping machanism inspired on Chain of Responsibility pattern. The global objects detection has been reduced considerably.
 - Analyzer: Part of its behavior and state has been moved to a new abstraction called "clusterization".
'.
		spec 
			package: 'Fuel' with: 'Fuel-MartinDias.338';
			package: 'FuelTests' with: 'FuelTests-MartinDias.84';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.68';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MartinDias.15';
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