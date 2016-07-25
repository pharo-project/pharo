baselines
baseline20beta2: spec 
	<version: '2.0-beta.2-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.squeaksource.com/MooseAlgos'.
		spec 
			package: 'Moose-Algos-Graph' ;
			package: 'Moose-Tests-Algos-Graph' ;
			package: 'Moose-Algos-Clustering' ;
			package: 'Moose-Algos-LinearAlgebra'.
		spec group: 'Tests' with: #(
						'Moose-Tests-Algos-Graph'
						"Dangerous now: Clustering and LinearAlgebra tests chrash the VM"). 
	]