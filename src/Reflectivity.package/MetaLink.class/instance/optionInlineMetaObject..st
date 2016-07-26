options
optionInlineMetaObject: aBoolean
	aBoolean
		ifTrue: [ options add: #optionInlineMetaObject ] 
		ifFalse: [ options remove: #optionInlineMetaObject ifAbsent:[] ]