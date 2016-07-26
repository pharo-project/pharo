versions
version20beta8: spec
	<version: '2.0-beta.8' imports: #('2.0-beta.3-baseline')>

	spec for: #common do: [
		spec author: 'jannik.laval'.
		spec timestamp: '6/1/2010 22:45'.
		spec blessing: #beta.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-tg.8';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-jannik_laval.15';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-jannik_laval.6';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-jannik_laval.4';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-cyrille_delaunay.8';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simon_denier.6';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-cyrille_delaunay.2';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-cyrille_delaunay.1';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-cyrille_delaunay.1'.].