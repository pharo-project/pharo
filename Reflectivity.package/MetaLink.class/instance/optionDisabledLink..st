options
optionDisabledLink: aBoolean
	aBoolean
		ifTrue: [ options add: #optionDisabledLink ] 
		ifFalse: [ options remove: #optionDisabledLink ifAbsent:[] ]