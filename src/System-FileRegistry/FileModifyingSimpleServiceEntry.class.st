"
I represent a service that may change the contents of a directory.
Such changes include:
* file creation
* file deletion
* file modification
"
Class {
	#name : #FileModifyingSimpleServiceEntry,
	#superclass : #SimpleServiceEntry,
	#category : #'System-FileRegistry'
}

{ #category : #'performing service' }
FileModifyingSimpleServiceEntry >> performServiceFor: anObject [
	| retval |
	retval := super performServiceFor: anObject.
	self changed: #fileListChanged.
	^retval	"is this used anywhere?"
]
