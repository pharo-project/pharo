stepping
stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."
	lastStepMessage ifNotNil:[
		(lastStepMessage receiver == aMorph and:[lastStepMessage selector == aSelector])
			ifTrue:[lastStepMessage := nil]].
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and:[stepMsg selector == aSelector]]).