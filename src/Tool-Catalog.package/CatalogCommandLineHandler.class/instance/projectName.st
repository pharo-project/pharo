accessing
projectName
	^ projectName ifNil: [ projectName := self argumentAt: 1 ]