accessing
fullName
	"Keeps a unique description for the receiver. As annotation to avoid converting each time is invoked"
	
	^self annotationNamed: self class fullNameKey
		ifAbsentPut:[ (self parentName, '->', self name) asSymbol ]