baselines
baseline20beta4: spec 
	<version: '2.0-beta.4-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.squeaksource.com/MooseAlgos'.
		spec 
			package: 'Moose-Algos-Graph' ;
			package: 'Moose-Tests-Algos-Graph' ;
			package: 'Moose-Tests-Algos-LinearAlgebra' ;
			package: 'Moose-Tests-Algos-Clustering' ;
			package: 'Moose-Tests-Algos-InformationRetrieval' ;
			package: 'Moose-Algos-Clustering' ;
			package: 'Moose-Algos-LinearAlgebra';
			package: 'Moose-Algos-FormalConceptAnalysis';
			package: 'Moose-Algos-InformationRetrieval'.
			"Dangerous now: Clustering and LinearAlgebra tests chrash the VM"
			spec group: 'Tests' with: #(
						'Moose-Tests-Algos-Graph' 'Moose-Tests-Algos-LinearAlgebra' 'Moose-Tests-Algos-Clustering' 'Moose-Tests-Algos-InformationRetrieval' ). 
	]