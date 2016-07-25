options
optionInlineCondition: aBoolean
	aBoolean
		ifTrue: [ options add: #optionInlineCondition ] 
		ifFalse: [ options remove: #optionInlineCondition ifAbsent:[] ]