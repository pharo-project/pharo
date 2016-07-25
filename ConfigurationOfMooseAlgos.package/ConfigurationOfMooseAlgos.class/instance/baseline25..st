baselines
baseline25: spec 
	<version: '2.5-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec description: 'Added traverser'.
		spec repository: 'http://www.smalltalkhub.com/mc/Moose/MooseAlgos/main'.
		spec 
			package: 'Moose-Algos-Graph' ;
			package: 'Moose-Tests-Algos-Graph' ;
			
			package: 'Moose-Algos-HierarchicalGraph' with: [spec requires: 'Moose-Algos-Graph'];
			package: 'Moose-Tests-Algos-HierarchicalGraph' ;
			
			package: 'Moose-Tests-Algos-InformationRetrieval' ;
			
			package: 'Moose-Algos-FormalConceptAnalysis';
			package: 'Moose-Tests-Algos-FormalConceptAnalysis';
			
			package: 'Moose-Algos-Lattice';
			package: 'Moose-Tests-Algos-Lattice';
			
			package: 'Moose-Algos-LinearAlgebra';
			package: 'Moose-Tests-Algos-LinearAlgebra' ;
			
			package: 'Moose-Algos-Clustering' with: [spec requires: 'Moose-Algos-LinearAlgebra'] ;
			package: 'Moose-Algos-InformationRetrieval';
			
			package: 'Moose-Tests-Algos-Clustering' with: [spec requires: 'Moose-Algos-LinearAlgebra'] ;
			
			package: 'Moose-Algos-Kontractor';
			package: 'Moose-Tests-Algos-Kontractor' ;
			
			package: 'Moose-Algos-GraphObjectTraverser' with: [spec requires: 'DeepTraverser'];

			project: 'DeepTraverser' with: [
				spec
					className: 'ConfigurationOfDeepTraverser';
					version: #development;
					file:  'ConfigurationOfDeepTraverser';
					repository: 'http://www.smalltalkhub.com/mc/Moose/DeepTraverser/main' ];
			
			package: 'CollectionExtensions' with: [spec repository: 'http://www.smalltalkhub.com/mc/Moose/CollectionExtensions/main'].
	
			spec group: 'Tests' with: #(
						'Moose-Tests-Algos-Graph'
						'Moose-Tests-Algos-LinearAlgebra'
						'Moose-Tests-Algos-Clustering'
						'Moose-Tests-Algos-InformationRetrieval'
						'Moose-Tests-Algos-FormalConceptAnalysis'
						'Moose-Tests-Algos-Kontractor'
						'Moose-Tests-Algos-Lattice'
						'Moose-Tests-Algos-HierarchicalGraph'). 
	]