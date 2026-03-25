Extension { #name : 'Context' }

{ #category : '*DebugInfo' }
Context >> readVariableNamed: aName [
	^ (self lookupVar: aName) readInContext: self
]

{ #category : '*DebugInfo' }
Context >> tempNamed: aName [
	"Returns the value of the temporaries, aName"

	| debugInfo |
	debugInfo := self method debugInfo.
	^ debugInfo readVariableNamed: aName fromContext: self
]

{ #category : '*DebugInfo' }
Context >> tempNamed: aName put: anObject [
	"Assign the value of the temp with name in aContext"
	| var |
	var := self lookupVar: aName.
	"To be checked. if we keep the error, we should raise a dedicated exception, 
	but no client would catch it, so why do we need it at all?"
 	var isLocalVariable ifFalse: [ ^self error: var name, ' is not a temp but, ', var class name].
	^var write: anObject inContext: self
]

{ #category : '*DebugInfo' }
Context >> temporaryVariableNames [

	"If the context is suspended on a send, we want to highlight the send.
	However, the context already executed the send, and the PC advanced to the next instruction.
	Thus, we want the instruction previously executed!"
	| suspendedPC |
	suspendedPC := self executedPC max: self method initialPC.
	^ self method debugInfo variableNamesAt: suspendedPC
]
