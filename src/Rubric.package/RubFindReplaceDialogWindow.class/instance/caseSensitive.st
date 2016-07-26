accessing
caseSensitive
	^ self model ifNil: [false] ifNotNil: [:m | m caseSensitive] 