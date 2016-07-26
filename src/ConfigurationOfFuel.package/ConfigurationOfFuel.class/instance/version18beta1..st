versions-1
version18beta1: spec 
	<version: '1.8-beta1' imports: #('1.8-baseline') >

	spec for: #pharo do: [
		spec blessing: #beta.
		spec description: '
		
		== visibles to the user:

- Metalevel serialization (serialization of classes, traits, compiled methods, etc) moved to a separate package FuelMetalevel. This makes Fuel core much smaller and eases the portability to other dialects such as Squeak and VW. It also makes sense, since almost nobody nobody uses "full" serialization for everything.
- Previously methods were completely serialized by default. This includes for example, SortedCollection. Now, by default, methods are serialized as globals. Special treatment for DoIt: all methods by default continue to be considered as global unless they are a #DoIt, in which case it is serialized fully.
- New special cluster for BlockClosures. Now "clean" closures avoid serializing the whole stack of contexts :) Thanks Juan Vuletich and Eliot Miranda.
- Weak references properly managed. Thanks Juan Vuletich and Levente.
- Fixed FLSerializer>>serialize:toFileNamed:. Thank you Norbert Hartl.
- Store index references more efficiently by using less bytes when possible. It means reduction in stream size
- Customize globals. Each user can add objects managed as global. It also lets the user to handle custom singletons.
- Manual migrations: class and variables renames.
- #visitSubstitution:by:
- #visitGlobalSend:name:selector:
- New FuelFileSystem compatibility package. It just adds a few extension methods to FileSystem streams. This way you can use Fuel with FileSystem streams out of the box.
- New package FuelCompression, which is an experiment using a gzip stream. We discover some bugs in the streams, but in addition, some some tests failed and one crash the VM. So, do not consider this a production ready. But if you want to give us a hand…
- New package FuelPreview for visualizating the object graph to serialize. This tool is based on Roassal. Thanks Tudor Girba and Alexander Bergel.
- Official "port to Squeak". With this version, Fuel (only the core) works out of the box in Squeak 4.2 and 4.3.
- This same version Fuel 1.8 can work in Pharo 1.1, 1.2, 1.3, 1.4, Squeak 4.2 and 4.3. You always need to load Fuel with Metacello and it will handle the compatibility packages.
- Fuel code was migrated from http://www.squeaksource.com/Fuel to http://ss3.gemstone.com/ss/Fuel

== not visibles:
- Added new LargeIdentityHashedCollection. This improves up to 20% serialization speed with large graphs. Thanks Levente!!!
- Merged 9 primitive clusters in 1 -> FLHookPrimitiveCluster.
- Rename DefaultMapper -> GeneralMapper
- Some optimization with #to:do:
- Removing streamFactory from both FLSerializer and FLMaterializer. They were not necessary and implementation simplifies a lot in this way.
- Reification of Encoder and Decoder: they delegate in just a few basic methods of a stream, making easy to port. It means Fuel just needs 2 methods for the write stream and 3 from read stream.
- Mappers directly map objects.
- FuelShouldIgnoreFuel.
- Added more and more benchmarks samples and test cases. Test coverage is about 90%.
- No direct access anymore to streams. Everything pass from Encoder and Decoder

		
		
		'.

		spec 
			package: 'Fuel' with: 'Fuel-MarianoMartinezPeck.615';
			package: 'FuelTests' with: 'FuelTests-MarianoMartinezPeck.214';
			package: 'FuelBenchmarks' with: 'FuelBenchmarks-MartinDias.105';
			package: 'FuelProgressUpdate' with: 'FuelProgressUpdate-MarianoMartinezPeck.39';
			package: 'FuelCompatibilityBeforePharo12' with: 'FuelCompatibilityBeforePharo12-MarianoMartinezPeck.6';
			package: 'FuelCompatibilityBeforePharo13' with: 'FuelCompatibilityBeforePharo13-MarianoMartinezPeck.2';
			package: 'FuelCompatibilityBeforePharo14' with: 'FuelCompatibilityBeforePharo14-MarianoMartinezPeck.1';
			package: 'FuelCompatibilityForSqueak' with: 'FuelCompatibilityForSqueak-MMP.4';
			package: 'FuelPackageLoader' with: 'FuelPackageLoader-MartinDias.52';
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