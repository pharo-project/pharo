versions
version20beta9: spec
	<version: '2.0-beta.9' imports: #('2.0-beta.4-baseline')>

	spec for: #common do: [
		spec author: 'jl'.
		spec timestamp: '09/13/2010 22:00'.
		spec blessing: #beta.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-simon_denier.9';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-jannik_laval.15';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-jannik_laval.6';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-jl.5';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-jl.10';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-Alexandre_Bergel.8';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-cyrille_delaunay.2';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-simon_denier.2'.].