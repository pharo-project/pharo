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
