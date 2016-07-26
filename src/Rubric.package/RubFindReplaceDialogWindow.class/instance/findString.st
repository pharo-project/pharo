accessing
findString
	^ self model ifNil: ['' asText] ifNotNil: [:m | m findString]