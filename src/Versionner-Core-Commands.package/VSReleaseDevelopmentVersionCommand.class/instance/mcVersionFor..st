actions
mcVersionFor: aVersionString
	| config |
	
	config := workflow project configurationClass.
	^ config project version: aVersionString