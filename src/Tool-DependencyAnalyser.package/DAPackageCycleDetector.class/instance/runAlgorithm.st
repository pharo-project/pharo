actions
runAlgorithm
	self stronglyConnectedComponents
		do: [ :scc | self findCycles: scc ]
		displayingProgress: 'finding cycles'