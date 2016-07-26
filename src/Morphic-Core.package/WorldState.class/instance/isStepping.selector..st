stepping
isStepping: aMorph selector: aSelector
	"Return true if the given morph is in the step list."
	lastStepMessage ifNotNil:[
		(lastStepMessage receiver == aMorph and:[lastStepMessage selector == aSelector])
			ifTrue:[^true]].
	stepList do:[:entry| (entry receiver == aMorph and:[entry selector == aSelector]) ifTrue:[^true]].
	^ false