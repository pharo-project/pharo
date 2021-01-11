"
I implement debug actions exposed by debuggers, and performed on the current debug session.
"
Class {
	#name : #StDebuggerActionModel,
	#superclass : #Object,
	#instVars : [
		'debugger',
		'session',
		'contextPredicate',
		'topContext'
	],
	#category : #'NewTools-Debugger-Model'
}

{ #category : #'instance creation' }
StDebuggerActionModel class >> on: aDebugSession [

	^ self basicNew
		  session: aDebugSession;
		  initialize
]

{ #category : #'debug - session' }
StDebuggerActionModel >> autoClassifyMessage: aMessage inClass: aClass [

	| method |
	method := aClass lookupSelector: aMessage selector.
	method protocol = Protocol unclassified ifTrue: [ 
		MethodClassifier classify: method ]
]

{ #category : #'debug - execution' }
StDebuggerActionModel >> clearDebugSession [
	contextPredicate := nil.
	self session terminate; clear
]

{ #category : #context }
StDebuggerActionModel >> computeInitialTopContext [

	topContext := session exception signalContext.
	topContext ifNil: [ 
		topContext := self interruptedContext.
		^ self ].
	(topContext hasContext: self interruptedContext) ifFalse: [ 
		topContext := self interruptedContext ]
]

{ #category : #accessing }
StDebuggerActionModel >> contextPredicate [

	^ contextPredicate ifNil: [ 
		  contextPredicate := self updateContextPredicateFor:
			                      self topContext ]
]

{ #category : #context }
StDebuggerActionModel >> contextPredicateFor: aContext [

	| postMortem ex |
	
	postMortem := self isContextPostMortem: aContext.

	(self contextSignalsAnException: aContext) ifFalse:[^ self predicateFor: aContext postMortem: postMortem].
	
	ex := aContext receiver.
	((ex isKindOf: TestFailure) ) ifTrue: [ 
		^ self predicateFor: aContext failure: ex postMortem: postMortem ].

	^ self predicateFor: aContext exception: ex postMortem: postMortem
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> contextSignalsAnException: aContext [

	^ (aContext receiver isKindOf: Exception) and: [ 
		  aContext selector = #signal ]
]

{ #category : #'debug - stack' }
StDebuggerActionModel >> copyStackToClipboard [
	Clipboard
		clipboardText:
			(String
				streamContents: [ :s | self interruptedContext shortDebugStackOn: s ])
]

{ #category : #'debug - stack' }
StDebuggerActionModel >> fileOutMethod: aMethod [
	aMethod methodClass fileOutMethod: aMethod selector
]

{ #category : #'debug - stack' }
StDebuggerActionModel >> filterStack: aStack [
	| context contextsToReject |	
	context := aStack first.
	contextsToReject := 0.

	[ 
	context notNil and: [ context ~~ self referenceContext  ] ] 
		whileTrue: [ 
			contextsToReject := contextsToReject + 1.
			context := context sender ].
	[ 
	context notNil and: [ 
		context method hasPragmaNamed: #debuggerCompleteToSender ] ] 
		whileTrue: [ 
			contextsToReject := contextsToReject + 1.
			context := context sender ].
	contextsToReject timesRepeat: [ aStack removeFirst ].
	^ aStack
]

{ #category : #'debug - session' }
StDebuggerActionModel >> implement: aMessage classified: messageCategory inClass: aClass forContext: aContext [

	self session
		implement: aMessage
		classified: messageCategory
		inClass: aClass
		forContext: aContext.
	self autoClassifyMessage: aMessage inClass: aClass.
	self updateTopContext
]

{ #category : #initialize }
StDebuggerActionModel >> initialize [
	super initialize.
	self computeInitialTopContext
]

{ #category : #accessing }
StDebuggerActionModel >> interruptedContext [
	^self session interruptedContext
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isContextAnAssertionFailure [

	^ self contextPredicate isContextAnAssertionFailure
]

{ #category : #context }
StDebuggerActionModel >> isContextPostMortem: aContext [

	aContext == topContext ifFalse: [ 
		^ self session isContextPostMortem: aContext ].

	"If the context is top context, we work on the interrupted context.
	The top context is not visible from the session"
	^ self session isContextPostMortem: self interruptedContext
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isContextSteppable [

	self interruptedContext ifNil: [ ^ false ].
	^ self contextPredicate isSteppable
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isInterruptedContextAnAssertEqualsFailure [

	^ self contextPredicate isContextAnAssertionFailure
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isInterruptedContextDoesNotUnderstand [
	^self contextPredicate isContextDoesNotUnderstand

]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isInterruptedContextMissingClassException [

	^ self contextPredicate isContextMissingClassException
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isInterruptedContextPostMortem [
	^ self contextPredicate isContextPostMortem
]

{ #category : #'debug - predicates' }
StDebuggerActionModel >> isInterruptedContextSubclassResponsibilityException [
	^ self contextPredicate isContextSubclassResponsibilityException
]

{ #category : #'debug - stack' }
StDebuggerActionModel >> peelToFirstLike: aContext [
	self session peelToFirstLike: aContext
]

{ #category : #context }
StDebuggerActionModel >> predicateFor: aContext exception: exception postMortem: isContextPostMortem [

	^ (StDebuggerErrorContextPredicate context: aContext)
		  exception: exception;
		  postMortem: isContextPostMortem;
		  yourself
]

{ #category : #context }
StDebuggerActionModel >> predicateFor: aContext failure: testFailure postMortem: isContextPostMortem [

	^ (StDebuggerTestFailureContextPredicate context: aContext)
		  failure: testFailure;
		  postMortem: isContextPostMortem;
		  yourself
]

{ #category : #context }
StDebuggerActionModel >> predicateFor: aContext postMortem: isContextPostMortem [

	^ (StDebuggerContextPredicate context: aContext)
		  postMortem: isContextPostMortem;
		  yourself
]

{ #category : #'debug - execution' }
StDebuggerActionModel >> proceedDebugSession [

	contextPredicate := nil.
	self session
		resume;
		clear
]

{ #category : #context }
StDebuggerActionModel >> referenceContext [
	"The reference context is the context that we wish to see in the debugger's stack.
	It it either the interrupted context if we are in a non-exception case,
	or the exception's signaler context in all other cases."
	self contextPredicate contextSignalsException ifFalse:[^self interruptedContext].
	^topContext receiver signalerContext 
]

{ #category : #'debug - execution' }
StDebuggerActionModel >> restartContext: aContext [
	self session restart: aContext.
	self updateTopContext
]

{ #category : #'debug - execution' }
StDebuggerActionModel >> returnValueFromExpression: aString fromContext: aContext [
	| value |
	value := session class compiler
		source: aString;
		context: aContext;
		receiver: aContext receiver;
		evaluate.
	self session returnValue: value from: aContext.
	self updateTopContext
]

{ #category : #'debug - execution' }
StDebuggerActionModel >> runToSelection: aSelectionInterval inContext: aContext [
	self session runToSelection: aSelectionInterval inContext: aContext.
	self updateTopContext
]

{ #category : #accessing }
StDebuggerActionModel >> session [
	^session
]

{ #category : #accessing }
StDebuggerActionModel >> session: aDebugSession [
	session := aDebugSession
]

{ #category : #context }
StDebuggerActionModel >> statusStringForContext [

	^ self contextPredicate printDescription
]

{ #category : #'debug - stepping' }
StDebuggerActionModel >> stepInto: aContext [ 
	self session stepInto: aContext.
	self updateTopContext
]

{ #category : #'debug - stepping' }
StDebuggerActionModel >> stepOver: aContext [
	self session stepOver: aContext.
	self updateTopContext
]

{ #category : #'debug - stepping' }
StDebuggerActionModel >> stepThrough: aContext [
	self session stepThrough: aContext.
	self updateTopContext
]

{ #category : #accessing }
StDebuggerActionModel >> topContext [
	^ topContext
]

{ #category : #context }
StDebuggerActionModel >> updateContextPredicate [
	^self updateContextPredicateFor: self topContext
]

{ #category : #context }
StDebuggerActionModel >> updateContextPredicateFor: aContext [    
	contextPredicate := self contextPredicateFor: aContext.
	^contextPredicate
]

{ #category : #context }
StDebuggerActionModel >> updateTopContext [

	topContext := self interruptedContext
]
