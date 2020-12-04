"
My job is to scan bytecodes to determine if a block is clean or not.

See #isClean
"
Class {
	#name : #CleanBlockChecker,
	#superclass : #InstructionClient,
	#instVars : [
		'isClean'
	],
	#category : #'Debugging-Core'
}

{ #category : #validation }
CleanBlockChecker class >> check: aMethod [
	^ self new check: aMethod
]

{ #category : #validation }
CleanBlockChecker >> check: aMethod [
	| scanner end |
	
	scanner := InstructionStream on: aMethod.
	end := aMethod endPC.

	[scanner pc <= end] whileTrue: [
		(self interpretNextInstructionUsing: scanner) ifFalse: [^false].
	].
	^true
]

{ #category : #initialization }
CleanBlockChecker >> interpretNextInstructionUsing: aScanner [ 
	
	isClean := true.
	aScanner interpretNextInstructionFor: self.
	^isClean
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> methodReturnConstant: offset [
	isClean := false
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> methodReturnReceiver [
	isClean := false
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> methodReturnTop [
	isClean := false
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> popIntoReceiverVariable: offset [
	isClean := false
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> pushReceiver [
	isClean := false
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> pushReceiverVariable: offset [
	isClean := false
]

{ #category : #'instruction decoding' }
CleanBlockChecker >> storeIntoReceiverVariable: offset [
	isClean := false
]
