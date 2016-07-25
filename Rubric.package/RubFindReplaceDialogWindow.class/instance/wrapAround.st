accessing
wrapAround
	^ self model ifNil: [true] ifNotNil: [:m | m wrapAround] 