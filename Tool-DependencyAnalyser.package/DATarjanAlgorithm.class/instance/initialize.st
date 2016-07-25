initialization
initialize
	super initialize.
	stronglyConnectedComponents := OrderedCollection new.
	stack := Stack new.
	runningIndex := 0
