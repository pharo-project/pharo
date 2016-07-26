versions
version23: spec
	<version: '2.3' imports: #('2.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'development'.
		spec description: 'First version on SmalltalkHub. Same as 2.2 on SqueakSource'.
		spec author: 'NicolasAnquetil'.
		spec timestamp: '2/15/2013 13:48'.
		spec 
			package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-janniklaval.16';
			package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-AlexandreBergel.18';
			package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-simon_denier.3';
			package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-simon_denier.2';
			package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-simondenier.4';
			package: 'Moose-Tests-Algos-FormalConceptAnalysis' with: 'Moose-Tests-Algos-FormalConceptAnalysis-usmanbhatti.3';
			package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-simon_denier.9';
			package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-simon_denier.17';
			package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-usmanbhatti.11';
			package: 'Moose-Algos-Lattice' with: 'Moose-Algos-Lattice-usmanbhatti.10';
			package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-simondenier.12';
			package: 'Moose-Algos-Kontractor' with: 'Moose-Algos-Kontractor-TudorGirba.2';
			package: 'Moose-Tests-Algos-Kontractor' with: 'Moose-Tests-Algos-Kontractor-TudorGirba.2';
			package: 'CollectionExtensions' with: 'CollectionExtensions-TudorGirba.32'. ].
