versions
version20beta5: spec
	<version: '2.0-beta.5' imports: #('2.0-beta.3-baseline')>

	spec for: #common do: [
		spec blessing: #beta.
		spec author: 'simondenier'.
		spec timestamp: '3/17/2010 22:55'.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-simondenier.4';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-cyrille_delaunay.10';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-cyrille_delaunay.4';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-cyrille_delaunay.2';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-cyrille_delaunay.5';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simondenier.4';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-cyrille_delaunay.1'.].