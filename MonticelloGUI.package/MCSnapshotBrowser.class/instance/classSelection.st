selecting
classSelection
	^ classSelection ifNil: [0] ifNotNil: [self visibleClasses indexOf: classSelection]