options
optionOneShot: aBoolean
	aBoolean
		ifTrue: [ options add: #optionOneShot ] 
		ifFalse: [ options remove: #optionOneShot ifAbsent:[] ]