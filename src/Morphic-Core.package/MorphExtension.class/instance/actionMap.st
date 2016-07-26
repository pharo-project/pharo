accessing
actionMap
	"Answer the value of actionMap"

	^actionMap ifNil: [self valueOfProperty: #actionMap ifAbsent: []]