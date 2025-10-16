"
I know hot to manipulate contexts and blocks for the fast step through functionality.
"
Class {
	#name : 'FastStepThroughController',
	#superclass : 'Object',
	#instVars : [
		'originalBlocks',
		'newBlocks'
	],
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'context operations' }
FastStepThroughController >> buildBlockContext: haltingBlock args: args sender: sender [
	" Create newContext from the originalBlock, and set its argument values if they exist. "

	| newContext |
	newContext := haltingBlock originalBlock asContextWithSender: sender.
	(args isNotNil and: [ haltingBlock originalBlock numArgs > 0 ]) ifTrue: [
			args isArray
				ifTrue: [ 1 to: haltingBlock originalBlock numArgs 
								do: [ :i | newContext tempAt: i put: (args at: i) ] ]
				ifFalse: [ newContext tempAt: 1 put: args ] ].
	^ newContext
]

{ #category : 'context operations' }
FastStepThroughController >> completeStep: aContext inProcess: aProcess [

	" Enable process interruptions triggered by HaltingBlocks. "
	| newContext |
	aProcess psValueAt: DebuggerSteppingState soleInstance index put: true. 

	" Run until aContext is on top, a return is reached, or an Error is signaled. "
	newContext := aProcess completeStep: aContext. 

	" Disable process interruptions triggered by HaltingBlocks. "
	aProcess psValueAt: DebuggerSteppingState soleInstance index put: false.

	^ newContext
]

{ #category : 'context operations' }
FastStepThroughController >> findContextForMethodHalfIfStepping: aContext [
	" Find the context for the method #haltIfStepping in aContext sender chain. "
	
	| ctx |
	ctx := aContext.
	[ ctx method = (HaltingBlock >> #haltIfStepping) ] 
		whileFalse: [ 
			ctx := ctx sender 
		].
	^ ctx
]

{ #category : 'context operations' }
FastStepThroughController >> findFirstContext: aContext withReceiverOfClass: aClass [

	| current receiverSeen |
	current := aContext.
	receiverSeen := false.

	[ current isNotNil and: [ receiverSeen = (current receiver class = aClass) ] ] whileTrue: [
			current := current sender.
			receiverSeen ifFalse: [ receiverSeen := current receiver class = aClass ] ].

	^ current
]

{ #category : 'context operations' }
FastStepThroughController >> findNextContext: aContext [
	" Build and return a newContext whose closure is the original block. "
	
	| current haltingBlock newContext args pair |
	
	" missing originalBlocks <=> missing haltingBlocks, and missing Halt means there's nothing to do. "
	( originalBlocks isEmpty or: [ (aContext receiver isKindOf: Halt) not ] ) 
	ifTrue: [ ^ aContext ].
	
	" get the halting block, the argument values, and the context sender to build newContext. "
	current :=  self findContextForMethodHalfIfStepping: aContext.
	haltingBlock := current receiver.
	
	pair := self getStackedArguments: current.
	current := pair first.
	args := pair second.

	newContext := self buildBlockContext: haltingBlock args: args sender: current.
	" step into the block "
	newContext stepToSendOrReturn.
	^ newContext
]

{ #category : 'context operations' }
FastStepThroughController >> findNextContext: originContext having: homeContext fixing: buggyContext [

	| blockContext fixedContext |
	"Find the next context, restoring blocks, removing HaltingBlocks"
	blockContext := self findNextContext: originContext.
	
	"a. Here step through exits the block is being evaluated."
	(self stepOutOfBlock: blockContext from: originContext to: homeContext) 
		ifNotNil: [:targetContext | ^ targetContext ].

	"b. Here step through behaves like step over."
	blockContext == originContext ifTrue: [ ^ originContext ].

	"c. Here step through steps into a block."
	fixedContext := self findFirstContext: buggyContext
								  withReceiverOfClass: HaltingBlock.
	blockContext sender: (fixedContext ifNotNil: [fixedContext sender ]).

	^ blockContext
]

{ #category : 'context operations' }
FastStepThroughController >> getStackedArguments: aContext [
	" Extract the arguments stacked during the evaluation 
	  of haltingBlock in aContext and its senders. "
	
	| args stackp ctx |
	ctx := aContext.
	args := stackp := nil.
	[ ctx receiver class = HaltingBlock ] whileTrue: [
			(ctx method selector == #valueWithArguments: and: [ (stackp := ctx stackPtr) > 0 ]) 
				ifTrue: [ args := ctx at: stackp ].
			ctx := ctx sender ].
	^ { ctx. args }
]

{ #category : 'initialization' }
FastStepThroughController >> initialize [

	super initialize.
	originalBlocks := OrderedCollection new.
	newBlocks := OrderedCollection new
]

{ #category : 'preparation' }
FastStepThroughController >> prepareContextForStepThrough: aContext [
	" Wrap every block from aContext with a HaltingBlock. 
	 Replace references to the originals by the wrappers in aContext. "
	
	| maybeABlock |
	1 to: aContext size do: [ :idx | self updateFullBlocksOf: aContext withIndex: idx ].

	aContext receiver class allSlots do: [ :aSlot |
			maybeABlock := aSlot read: aContext receiver.
			maybeABlock isBlock 
				ifTrue: [ self updateFullBlockOfReceiver: aContext receiver block: maybeABlock slot: aSlot ] ]
]

{ #category : 'cleanup' }
FastStepThroughController >> revertBlocks [

	newBlocks asArray elementsForwardIdentityTo: originalBlocks asArray
]

{ #category : 'context operations' }
FastStepThroughController >> stepOutOfBlock: blockContext from: originContext to: targetContext [
	" Return the context obtained after stepping out of the blockContext, 
	but only if this step through corresponds to the moment when the block 
	is exited. Otherwise return nil"

	| target current block origin |
	block := blockContext.
	origin := originContext.
	target := targetContext.

	(block == origin and: [ block isNotNil and: [ origin home ~= target and: [ origin hasSender: target ] ] ]) ifTrue: [
			current := origin.
			[ current = target ] whileFalse: [ current := current sender ].
			^ current ]. "Return nil if the current step through does not exit the block"
	^ nil
]

{ #category : 'preparation' }
FastStepThroughController >> transformBlock: originalBlock [

	| blockIndex newBlock |
	(blockIndex := originalBlocks indexOf: originalBlock) > 0 
		ifTrue: [ ^ newBlocks at: blockIndex ].

	newBlock := HaltingBlock new
		            originalBlock: originalBlock;
		            yourself.

	originalBlocks add: originalBlock.
	newBlocks add: newBlock.

	^ newBlock
]

{ #category : 'preparation' }
FastStepThroughController >> updateFullBlockOfReceiver: aReceiver block: originalBlock slot: aSlot [

	aSlot write: (self transformBlock: originalBlock) to: aReceiver
]

{ #category : 'preparation' }
FastStepThroughController >> updateFullBlocksOf: aContext withIndex: anIndex [

	| originalBlock |
	originalBlock := aContext at: anIndex.
	originalBlock isBlock ifFalse: [ ^ self ].

	aContext at: anIndex put: (self transformBlock: originalBlock)
]
