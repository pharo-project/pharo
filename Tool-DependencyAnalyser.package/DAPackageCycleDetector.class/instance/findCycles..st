actions
findCycles: stronglyConnectedComponents
	stronglyConnectedComponents packages
		do: [ :package | 
			self findAllCyclesFrom: stronglyConnectedComponents for: package 
			]