accessing
isRegex
	^ self model ifNil: [false] ifNotNil: [:m | m isRegex] 