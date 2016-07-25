model access
convertTo: aClass
	"Set the converter object class."

	self converter isNil
		ifTrue: [self converter: (ObjectStringConverter forClass: aClass)]
		ifFalse: [self converter objectClass: aClass]