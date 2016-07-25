versions
version206: spec
	<version: '2.0.6' imports: #('2.0-beta.3-baseline')>

	spec for: #common do: [
		spec author: 'simondenier'.
		spec timestamp: '4/6/2010 19:41'.
		spec blessing: #development.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-jl.7';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-cyrille_delaunay.10';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-cyrille_delaunay.4';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-cyrille_delaunay.2';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-cyrille_delaunay.8';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simon_denier.6';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-cyrille_delaunay.1'.].