Class {
	#name : 'EnhancedDebugSessionWithHaltingBlock',
	#superclass : 'DebugSession',
	#instVars : [
		'originalBlocks',
		'newBlocks'
	],
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'menu - operations' }
EnhancedDebugSessionWithHaltingBlock >> findNextContext: aContext [ 
	
	| callingContext idx haltingBlock newContext |
	
	originalBlocks ifEmpty: [ ^ aContext ].
	
	(aContext receiver isKindOf: Halt) ifFalse: [ ^ aContext ].

	callingContext := aContext.
	[ callingContext method = (HaltingBlock >> #haltIfStepping)  ] whileFalse: [ callingContext := callingContext sender ].

	haltingBlock := callingContext receiver.
	[ callingContext receiver class = HaltingBlock  ] whileTrue: [ callingContext := callingContext sender ].
	
	newContext := haltingBlock originalBlock asContextWithSender: callingContext.
	newContext stepToSendOrReturn.

	^ newContext
]

{ #category : 'initialization' }
EnhancedDebugSessionWithHaltingBlock >> initialize [ 
	
	super initialize.
	originalBlocks := OrderedCollection new.
	newBlocks := OrderedCollection new.
]

{ #category : 'preparation' }
EnhancedDebugSessionWithHaltingBlock >> prepareContextForStepThrough: aContext [ 
	
	1 to: aContext size do: [ :idx | 
		self updateFullBlocksOf: aContext withIndex: idx ]
]

{ #category : 'clean-up' }
EnhancedDebugSessionWithHaltingBlock >> revertBlocks [

	newBlocks asArray elementsForwardIdentityTo: originalBlocks asArray.
	
]

{ #category : 'debugging actions' }
EnhancedDebugSessionWithHaltingBlock >> stepThrough: aContext [
	"Send messages until you return to selectedContext.
	 Used to step into a block in the method."

	| newContext |

	aContext stepIntoQuickMethod: false.
	(self isContextPostMortem: aContext) ifTrue: [^ self].
	
	self prepareContextForStepThrough: aContext.
	
	interruptedProcess psValueAt: (DebuggerSteppingState soleInstance index) put: true.
	newContext := interruptedProcess completeStep: aContext.
	interruptedProcess psValueAt: (DebuggerSteppingState soleInstance index) put: false.

	newContext := self findNextContext: newContext.

	self revertBlocks.

	self updateContextTo:
		(newContext == aContext
			ifTrue: [ (self stepToFirstInterestingBytecodeIn: interruptedProcess) ]
			ifFalse: [ newContext ]).

	self triggerEvent: #stepThrough
]

{ #category : 'preparation' }
EnhancedDebugSessionWithHaltingBlock >> updateFullBlocksOf: aContext withIndex: anIndex [ 
	
	| originalBlock newBlock blockIndex |
	
	originalBlock := aContext at: anIndex.
	originalBlock isBlock ifFalse: [ ^ self ].

	(blockIndex := originalBlocks indexOf: originalBlock) > 0
		ifTrue: [ 
			aContext at: anIndex put: (newBlocks at: blockIndex). 
			^ self ].
	
	newBlock := HaltingBlock new 
		originalBlock: originalBlock;
		yourself.
			
	originalBlocks add: originalBlock.
	newBlocks add: newBlock.
	
	aContext at: anIndex put: newBlock.
]
