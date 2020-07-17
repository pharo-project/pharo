"
If aContext has among its senders a context for the #do: method of an instance of Collection (or subclasses), step until the next time the argument block of #do: is executed (or until the #do: context is popped if it happens first)
"
Class {
	#name : #StepToNextDoIterationDbgScript,
	#superclass : #SindarinScript,
	#instVars : [
		'startContext',
		'scdbg',
		'doContext',
		'doArgumentBlock'
	],
	#category : #Sindarin
}

{ #category : #example }
StepToNextDoIterationDbgScript class >> doCollectionExample [
	|a|
	a := OrderedCollection new.
	{ 1 . 2 . 3 } do: [ :int | a add: int. a add: int ].
]

{ #category : #accessing }
StepToNextDoIterationDbgScript class >> scriptName [
	^ 'StepToNextDoIteration'
]

{ #category : #helper }
StepToNextDoIterationDbgScript >> context: aContext isForBlock: aBlock [
	"Returns whether aContext is a block context for aBlock"
	aContext isBlockContext ifFalse: [ ^ false ].
	^ aContext closure sourceNode == aBlock sourceNode.
]

{ #category : #helper }
StepToNextDoIterationDbgScript >> contextIsOnStack: aContext [
	"Returns whether aContext is somewhere on the stack"
	^ (scdbg context findContextSuchThat: [ :aContexta| aContexta == aContext ]) isNotNil.
]

{ #category : #running }
StepToNextDoIterationDbgScript >> execute: aScriptableDebugger [
	| stepHook |
	scdbg := aScriptableDebugger.
	startContext := aScriptableDebugger context.
	"doContext is the context for the #do: message send to an instance of Collection (or subclass)"
	doContext := self findContextForSelector: #do: andClassOrSubclassOf: Collection.
	doContext ifNil: [ ^false ].
	"doArgumentBlock is the block that has been given to the #do: message as argument. It is the block to be executed on each element of the collection"
	doArgumentBlock := doContext arguments at: 1.
	"Add a step hook that will raise a ContextNotOnStack exception if at any point, the doContext is no longer on the stack (this can happens when exceptions are involved)"
	stepHook := [ (self contextIsOnStack: doContext) ifFalse: [ ContextNotOnStack signal ] ].
	scdbg addStepHook: stepHook.
	[
		"case 1: startContext is above or equal to a block context for the do:'s block"
		(scdbg context findContextSuchThat: [:ctxt | self context: ctxt isForBlock: doArgumentBlock]) ifNotNil: [ 
			scdbg stepToContext: doContext.
			self stepToContextForBlock: doArgumentBlock.
			scdbg removeStepHook: stepHook.
			^true. 
		].
		"case 2: startContext is above the doContext and there is no block context for doArgumentBlock between them"
		self stepToContextForBlock: doArgumentBlock.
		scdbg removeStepHook: stepHook.
		^ true.
	] on: ContextNotOnStack do: [
		scdbg removeStepHook: stepHook.
		^ self
	].
]

{ #category : #running }
StepToNextDoIterationDbgScript >> execute: aScriptableDebugger withArgs: anOrderedCollection [
	self execute: aScriptableDebugger.
]

{ #category : #helper }
StepToNextDoIterationDbgScript >> findContextForSelector: aSymbol andClassOrSubclassOf: aClass [
	"Run through the context stack from top to bottom to find a context whose selector is aSymbol and for which the class of its receiver is equal to or a subclass of aClass"
	^ scdbg context findContextSuchThat: [ :ctxt | (ctxt method selector = aSymbol) and: [ ctxt receiver isKindOf: aClass  ] ]
]

{ #category : #helper }
StepToNextDoIterationDbgScript >> stepToContextForBlock: aBlock [
	"Runs the execution until the current context is a block context for aBlock"
	[ self context: (scdbg context) isForBlock: aBlock ] whileFalse: [scdbg step].
]
