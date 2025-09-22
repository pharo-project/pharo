"
I know hot to manipulate contexts and blocks for the fast step through functionality.
"
Class {
	#name : #FastStepThroughController,
	#superclass : #Object,
	#instVars : [
		'originalBlocks',
		'newBlocks'
	],
	#category : #'Debugger-Model-Core'
}

{ #category : #'menu - operations' }
FastStepThroughController >> findNextContext: aContext [ 
	
	| callingContext haltingBlock newContext |

	originalBlocks ifEmpty: [ ^ aContext ].
	
	(aContext receiver isKindOf: Halt) ifFalse: [ ^ aContext ].

	callingContext := aContext.
	[ callingContext method = (HaltingBlock >> #haltIfStepping) ] whileFalse: [ callingContext := callingContext sender ].

	haltingBlock := callingContext receiver.
	
	[ callingContext receiver class = HaltingBlock  ] whileTrue: [ callingContext := callingContext sender ].
	
	newContext := haltingBlock originalBlock asContextWithSender: callingContext.
	newContext stepToSendOrReturn.

	^ newContext
]

{ #category : #initialization }
FastStepThroughController >> initialize [ 
	
	super initialize.
	originalBlocks := OrderedCollection new.
	newBlocks := OrderedCollection new.
]

{ #category : #preparation }
FastStepThroughController >> prepareContextForStepThrough: aContext [ 
	
	| maybeABlock |
	1 to: aContext size do: [ :idx | 
		self updateFullBlocksOf: aContext withIndex: idx ].
	
	aContext receiver class allSlots do: [ :aSlot | 
		maybeABlock := aSlot read: aContext receiver.
		maybeABlock isBlock 
			ifTrue: [ self updateFullBlockOfReceiver: aContext receiver block: maybeABlock slot: aSlot ] ]
]

{ #category : #'clean-up' }
FastStepThroughController >> revertBlocks [

	newBlocks asArray elementsForwardIdentityTo: originalBlocks asArray.
	
]

{ #category : #preparation }
FastStepThroughController >> transformBlock: originalBlock [

	| blockIndex newBlock |
	(blockIndex := originalBlocks indexOf: originalBlock) > 0
		ifTrue: [ 
			^ (newBlocks at: blockIndex). ].
	
	newBlock := HaltingBlock new 
		originalBlock: originalBlock;
		yourself.
			
	originalBlocks add: originalBlock.
	newBlocks add: newBlock.

	^ newBlock	
]

{ #category : #preparation }
FastStepThroughController >> updateFullBlockOfReceiver: aReceiver block: originalBlock slot: aSlot [ 
	
	aSlot write: (self transformBlock: originalBlock) to: aReceiver.
]

{ #category : #preparation }
FastStepThroughController >> updateFullBlocksOf: aContext withIndex: anIndex [ 
	
	| originalBlock |
	
	originalBlock := aContext at: anIndex.
	originalBlock isBlock ifFalse: [ ^ self ].
	
	aContext at: anIndex put: (self transformBlock: originalBlock).
]
