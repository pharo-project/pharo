ui process
defer: aBlock
	" Evaluate the given Block in the UI thread as soon as there is nothing scheduled. Evaluate immediately when there is no UI"
	aBlock value