"
My job is to scan bytecodes for instance variable references.

BlockContext allInstances collect: [ :x |
	{x. x hasInstVarRef}
].
"
Class {
	#name : 'InstVarRefLocator',
	#superclass : 'InstructionClient',
	#instVars : [
		'bingo'
	],
	#category : 'Debugging-Core',
	#package : 'Debugging-Core'
}

{ #category : 'validation' }
InstVarRefLocator class >> check: aMethod [
	^ self new check: aMethod
]

{ #category : 'validation' }
InstVarRefLocator >> check: aMethod [
	| scanner end |
	scanner := InstructionStream on: aMethod.
	end := aMethod endPC.

	[scanner pc <= end] whileTrue: [
		(self interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false
]

{ #category : 'initialization' }
InstVarRefLocator >> interpretNextInstructionUsing: aScanner [ 
	
	bingo := false.
	aScanner interpretNextInstructionFor: self.
	^bingo
]

{ #category : 'instruction decoding' }
InstVarRefLocator >> popIntoReceiverVariable: offset [ 

	bingo := true
]

{ #category : 'instruction decoding' }
InstVarRefLocator >> pushReceiverVariable: offset [

	bingo := true
]

{ #category : 'instruction decoding' }
InstVarRefLocator >> storeIntoReceiverVariable: offset [ 
	bingo := true
]
