accessing
findText
	^ self model ifNil: ['' asText] ifNotNil: [:m | m findText]