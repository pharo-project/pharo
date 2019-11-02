Extension { #name : #RBProgramNode }

{ #category : #'*NECompletion' }
RBProgramNode >> completionEntries [
	^ #()
]

{ #category : #'*NECompletion' }
RBProgramNode >> completionToken [
	^ String empty
]

{ #category : #'*NECompletion' }
RBProgramNode >> select: aCollection beginningWith: aString [
	"note: to be inlined into the senders"
	^ aCollection select: [ :each | each beginsWith: aString asString ]
]
