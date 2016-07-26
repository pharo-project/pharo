options
optionMetalevel: aBoolean
	aBoolean
		ifTrue: [ options add: #optionMetalevel ] 
		ifFalse: [ options remove: #optionMetalevel ifAbsent:[] ]