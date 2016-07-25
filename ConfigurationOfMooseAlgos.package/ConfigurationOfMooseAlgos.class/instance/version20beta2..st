versions
version20beta2: spec
	<version: '2.0-beta.2' imports: #('2.0-beta.2-baseline')>

	spec for: #common do: [
		spec blessing: #beta.
		spec author: 'tg'.
		spec timestamp: '12/28/2009 21:12'.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-simon_denier.1';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simon_denier.1';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-simon_denier.1';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-simon_denier.1'.].