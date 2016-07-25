versions
version20beta3: spec
	<version: '2.0-beta.3' imports: #('2.0-beta.2-baseline')>

	spec for: #common do: [
		spec author: 'simondenier'.
		spec timestamp: '1/18/2010 14:28'.
		spec blessing: #beta.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-simondenier.2';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-simondenier.2';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-simon_denier.1';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-simon_denier.1'.].