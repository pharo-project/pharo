baselines
legacyDefault: spec 
	<version: 'legacyDefault'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.squeaksource.com/MooseAlgos'.
		spec 
			package: 'Moose-Algos-Graph' ;
			package: 'Moose-Tests-Algos-Graph' ;
			package: 'Moose-Tests-Algos-LinearAlgebra' ;
			package: 'Moose-Tests-Algos-Clustering' ;
			package: 'Moose-Tests-Algos-InformationRetrieval' ;
			package: 'Moose-Tests-Algos-FormalConceptAnalysis';
			package: 'Moose-Algos-Clustering' ;
			package: 'Moose-Algos-LinearAlgebra';
			package: 'Moose-Algos-FormalConceptAnalysis';
			package: 'Moose-Algos-Lattice';
			package: 'Moose-Algos-InformationRetrieval';
			package: 'Moose-Algos-Kontractor';
			package: 'Moose-Tests-Algos-Kontractor'.
			spec group: 'Tests' with: #(
						'Moose-Tests-Algos-Graph'
						'Moose-Tests-Algos-LinearAlgebra'
						'Moose-Tests-Algos-Clustering'
						'Moose-Tests-Algos-InformationRetrieval'
						'Moose-Tests-Algos-FormalConceptAnalysis'
						'Moose-Tests-Algos-Kontractor'). 
	]