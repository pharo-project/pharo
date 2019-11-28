Extension { #name : #RBProgramNode }

{ #category : #'*NECompletion' }
RBProgramNode >> completionEntries: offset [
	"for now we give all selectors as a fallback, maybe we should add variables, too"
	^Symbol allSelectors 
		select: [ :each | each beginsWith: (self completionToken: offset)] 
		thenCollect: [ :each | NECSymbolEntry contents: each node: self ]

]

{ #category : #'*NECompletion' }
RBProgramNode >> completionToken: offset [
	"fall back: if we are not able to parse to something better, we use the word before the cursor"
	^self methodNode sourceCode wordBefore: offset

]
