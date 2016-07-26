versions
version32: spec
	<version: '3.2' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Order the entries for redo and undo operations using the timestamp. 
				(Much better than using the user selection order).
			- Implemented #asEpiceaRingDefinition in metamodel and ring definitions. 
				The advantages are: 
				- Store package name always, instead of either nil or the RPackage.
				- Huge speed up in ChangeSet importation since it was storing parent of the methods.
				- It is possible to Redo or Undo an event imported from a ChangeSet (it was almost impossible).
				- Also do not store #passive annotation on each method, which saves a bit of space.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '04/03/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.129';
			package: 'Epicea' with: 'Epicea-MartinDias.531'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].