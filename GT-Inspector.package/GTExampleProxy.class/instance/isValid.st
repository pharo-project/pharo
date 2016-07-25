testing
isValid
	^ self withExampleDo: [ self example isValid ] ifAbsent: [ false ]