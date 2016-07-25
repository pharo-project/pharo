performing service
performServiceFor: anObject
	| retval |
	retval := super performServiceFor: anObject.
	self changed: #fileListChanged.
	^retval	"is this used anywhere?"