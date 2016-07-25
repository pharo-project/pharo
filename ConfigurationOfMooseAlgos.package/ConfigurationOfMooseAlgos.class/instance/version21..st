versions
version21: spec 
        <version: '2.1' imports: #('2.0-beta.4-baseline')>

        spec for: #common do: [
                spec blessing: #release.
			spec package: 'Moose-Algos-Graph' with: 'Moose-Algos-Graph-simon_denier.14'.
			spec package: 'Moose-Algos-LinearAlgebra' with: 'Moose-Algos-LinearAlgebra-simon_denier.17'.
			spec package: 'Moose-Algos-Clustering' with: 'Moose-Algos-Clustering-simon_denier.9'.
			spec package: 'Moose-Algos-FormalConceptAnalysis' with: 'Moose-Algos-FormalConceptAnalysis-simon_denier.6'.
			spec package: 'Moose-Algos-InformationRetrieval' with: 'Moose-Algos-InformationRetrieval-simon_denier.11'.
			spec package: 'Moose-Tests-Algos-Graph' with: 'Moose-Tests-Algos-Graph-AlexandreBergel.13'.
			spec package: 'Moose-Tests-Algos-LinearAlgebra' with: 'Moose-Tests-Algos-LinearAlgebra-simon_denier.3'.
			spec package: 'Moose-Tests-Algos-Clustering' with: 'Moose-Tests-Algos-Clustering-simon_denier.2'.
			spec package: 'Moose-Tests-Algos-InformationRetrieval' with: 'Moose-Tests-Algos-InformationRetrieval-simon_denier.3'.
                ].