Extension { #name : #RBProgramNode }

{ #category : #'*NECompletion' }
RBProgramNode >> completionDescription [
	"TODO: we need to implement this to return an empty description here and something useful for all the cases like sens, symbols, variables..."
	^NECEntryDescription 
					label: 'label depends on node'
					title: 'class or something else, depends on node'
					description: 'here the class comment or method source'
]

{ #category : #'*NECompletion' }
RBProgramNode >> completionToken [
	^ String empty
]
