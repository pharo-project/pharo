Extension { #name : #Trait }

{ #category : #'*System-Changes' }
Trait >> removeFromChanges [
	"References to the receiver, a class, and its metaclass should no longer be included in the system ChangeSet."

	self class environment at: #ChangeSet ifPresent: [ :changeSet |
		changeSet current removeClassAndMetaClassChanges: self ].
]
