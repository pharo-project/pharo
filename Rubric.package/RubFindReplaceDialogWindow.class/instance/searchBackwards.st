accessing
searchBackwards
	^ self model ifNil: [false] ifNotNil: [:m | m searchBackwards]