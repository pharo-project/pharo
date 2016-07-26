instance creation
newWith: oldRelationGraph on: newRelationGraph
	^ self new
		initializeWith: oldRelationGraph on: newRelationGraph;
		yourself.