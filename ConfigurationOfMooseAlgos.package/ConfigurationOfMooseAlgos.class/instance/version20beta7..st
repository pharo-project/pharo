versions
version20beta7: spec
	<version: '2.0-beta.7' imports: #('2.0-beta.3-baseline')>

	spec for: #common do: [
		spec author: 'simondenier'.
		spec timestamp: '4/8/2010 11:11'.
		spec blessing: #beta.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-jl.7';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-cyrille_delaunay.13';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-cyrille_delaunay.4';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-cyrille_delaunay.2';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-cyrille_delaunay.8';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simon_denier.6';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-cyrille_delaunay.2';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-cyrille_delaunay.1'.].