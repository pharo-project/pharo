Extension { #name : #TClass }

{ #category : #'*System-Changes' }
TClass >> removeFromChanges [
	"References to the receiver, a class, and its metaclass should no longer be included in the system ChangeSet."

	self class environment at: #ChangeSet ifPresent: [ :changeSet |
		changeSet current removeClassAndMetaClassChanges: self ].
]
