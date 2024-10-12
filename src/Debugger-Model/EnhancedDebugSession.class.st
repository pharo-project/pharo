Class {
	#name : 'EnhancedDebugSession',
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
EnhancedDebugSession >> findNextContext: aContext [ 
	
	| blockContext idx |
	originalBlocks ifEmpty: [ ^ aContext ].
	
	(aContext receiver isKindOf: UnknownBytecode) ifFalse: [ ^ aContext ].
	aContext receiver bytecode = self bytecodeForHalt ifFalse: [ ^ aContext ].

	"Climb up the context, if I found nil, just return the original context"
	(blockContext := aContext sender) ifNil: [ ^aContext ].
	(blockContext := blockContext sender) ifNil: [ ^aContext ].
	(blockContext := blockContext sender) ifNil: [ ^aContext ].
	(blockContext := blockContext sender) ifNil: [ ^aContext ].

	idx := newBlocks indexOf: blockContext closure ifAbsent: [ ^ aContext ].

	blockContext privSetClosure: (originalBlocks at: idx).
	blockContext privSetMethod: (originalBlocks at: idx) compiledBlock.
	blockContext stepToSendOrReturn.
	
	^ blockContext

]

{ #category : 'debugging actions' }
EnhancedDebugSession >> stepOver: aContext [
	"Send the selected message in selectedContext, and regain control
	after the invoked method returns."

	| newContext |
	aContext stepIntoQuickMethod: false.
	(self isContextPostMortem: aContext) ifTrue: [^ self].

	newContext := interruptedProcess completeStep: aContext.
	self updateContextTo:
		(newContext == aContext
			ifTrue: [ (self stepToFirstInterestingBytecodeIn: interruptedProcess) ]
			ifFalse: [ newContext ]).

	self triggerEvent: #stepOver
]

{ #category : 'debugging actions' }
EnhancedDebugSession >> stepThrough: aContext [
	"Send messages until you return to selectedContext.
	 Used to step into a block in the method."

	| newContext |

	aContext stepIntoQuickMethod: false.
	(self isContextPostMortem: aContext) ifTrue: [^ self].
	
	self prepareContextForStepThrough: aContext.
	
	newContext := interruptedProcess completeStep: aContext.

	newContext := self findNextContext: newContext.

	self revertBlocks.

	self updateContextTo:
		(newContext == aContext
			ifTrue: [ (self stepToFirstInterestingBytecodeIn: interruptedProcess) ]
			ifFalse: [ newContext ]).

	self triggerEvent: #stepThrough
]
