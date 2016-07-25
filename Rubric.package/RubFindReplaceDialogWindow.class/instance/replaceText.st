accessing
replaceText
	^ self model ifNil: [ '' asText] ifNotNil: [:m | m replaceText]