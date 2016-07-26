running
findCircuits
	| tarjanCircuits |
	tarjanCircuits := MalTarjan new
		nodesAndNextFrom: nodes;
		run;
		circuits.
	circuits := tarjanCircuits 
		collect: [ :circuit | 
			circuit collect: [ :each | self findNode: each model ] ]