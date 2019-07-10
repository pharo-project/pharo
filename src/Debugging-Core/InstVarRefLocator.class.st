"
My job is to scan bytecodes for instance variable references.

BlockContext allInstances collect: [ :x |
	{x. x hasInstVarRef}
].
"
Class {
	#name : #InstVarRefLocator,
	#superclass : #InstructionClient,
	#instVars : [
		'bingo'
	],
	#category : #'Debugging-Core'
}

{ #category : #initialization }
InstVarRefLocator >> interpretNextInstructionUsing: aScanner [ 
	
	bingo := false.
	aScanner interpretNextInstructionFor: self.
	^bingo
]

{ #category : #'instruction decoding' }
InstVarRefLocator >> popIntoReceiverVariable: offset [ 

	bingo := true
]

{ #category : #'instruction decoding' }
InstVarRefLocator >> pushReceiverVariable: offset [

	bingo := true
]

{ #category : #'instruction decoding' }
InstVarRefLocator >> storeIntoReceiverVariable: offset [ 
	bingo := true
]
