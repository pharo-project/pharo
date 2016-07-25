stepping
isStepping: aMorph
	"Return true if the given morph is in the step list."
	lastStepMessage ifNotNil:[(lastStepMessage receiver == aMorph) ifTrue:[^true]].
	stepList do:[:entry| entry receiver == aMorph ifTrue:[^true]].
	^ false