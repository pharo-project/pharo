Extension { #name : #CommandLineUIManager }

{ #category : #'*Monticello' }
CommandLineUIManager >> merge: merger informing: aString [
	"Check for conflicts, if there are none, just continue. 
	 Merger will accept all conflicts"
	merger hasConflicts ifFalse: [ ^ self ].
	self abort: aString title: 'Conflict detected'
]
