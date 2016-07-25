versions
version26: spec
	<version: '2.6' imports: #('2.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'For Moose 5.1'.
		spec author: 'AndreiChis'.
		spec timestamp: '5/18/2015 00:41'.
		spec 
			project: 'DeepTraverser' with: #stable.
		spec 
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-AndreHora.10';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-usmanbhatti.11';
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-ChristopheDemarey.30';
			package: 'Moose-Algos-GraphObjectTraverser' with: 'Moose-Algos-GraphObjectTraverser-ChristopheDemarey.5';
			package: 'Moose-Algos-HierarchicalGraph' with: 'Moose-Algos-HierarchicalGraph-DennisSchenk.22';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-VincentBlondeau.13';
			package: 'Moose-Algos-Kontractor' with: 'Moose-Algos-Kontractor-LeoPerard.4';
			package: 'Moose-Algos-Lattice' with: 'Moose-Algos-Lattice-VincentBlondeau.11';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-TudorGirba.21';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-AndreHora.4';
			package: 'Moose-Tests-Algos-FormalConceptAnalysis' with: 'Moose-Tests-Algos-FormalConceptAnalysis-AlexandreBergel.6';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-ChristopheDemarey.33';
			package: 'Moose-Tests-Algos-HierarchicalGraph' with: 'Moose-Tests-Algos-HierarchicalGraph-DennisSchenk.14';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-AndreHora.5';
			package: 'Moose-Tests-Algos-Kontractor' with: 'Moose-Tests-Algos-Kontractor-TudorGirba.5';
			package: 'Moose-Tests-Algos-Lattice' with: 'Moose-Tests-Algos-Lattice-AndreHora.9';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-AndreHora.4';
			package: 'CollectionExtensions' with: 'CollectionExtensions-TudorGirba.41'. ].
