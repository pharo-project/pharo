versions
version20beta1: spec
	<version: '2.0-beta.1' imports: #('2.0-beta.1-baseline')>

	spec for: #common do: [
		spec author: 'tg'.
		spec timestamp: '11/22/2009 11:21'.
		spec project: 'Mondrian for MooseAlgos' with: '2.0-beta.1'.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-simon_denier.1';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simon_denier.1';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-simon_denier.1';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-simon_denier.1'.].