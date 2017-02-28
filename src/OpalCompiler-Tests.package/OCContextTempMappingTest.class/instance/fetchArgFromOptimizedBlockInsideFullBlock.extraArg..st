tests
fetchArgFromOptimizedBlockInsideFullBlock: anArg extraArg: anotherArg
	
	| result |
	self class withAllSubclassesDo: [ :each | 
		result := anArg = anArg ifTrue: [ anotherArg value. thisContext tempNamed: #anArg ]].
	^result