examples - branches
exampleIfNilNested
	^ true ifNil: [ (false ifNil: [ ^ true ]) not ]