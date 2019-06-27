"
Halt is a special exception provided to support breakpoints as defined in  Object>>halt.

!! Basic usage

To manually set a breakpoint you can use message halt as follows: 
[[[			
anObject halt. 
]]]
You can also use 

[[[
Halt now
]]]

[[[
Halt now: 'With a nice explanation'
]]]

!! Advanced usage
The class Halt supports different halting mechanisms such as conditionals, based on iteration or simply stopping the first time. 

!!! Conditionals

[[[
foo
	...
	Halt if: #invokedFromThisSelector
]]]

This expression will only stop the execution  of method foo if this method is in the call chain (is invoked from ) message invokedFromThisSelector.

[[[
foo
      ...
      Halt if: aBlock		
]]]

The execution of foo will stop if aBlock returns true. 

!!! Counting
Sometimes we need to stop after a given number of iteration.

[[[
Halt count: 5
]]]



!!! Once
It is really useful sometimes to stop only and only one time

[[[
foo
	...
	Halt once. 
]]]

It will be stopped the first time. 
Then if you need to rearm it to stop another time you should use

[[[
Halt resetOnce			
]]]



"
Class {
	#name : #Halt,
	#superclass : #Exception,
	#category : #'Kernel-Exceptions'
}

{ #category : #halting }
Halt class >> fromCount: anInteger [
	"Always halt after a count has been reached"

	<debuggerCompleteToSender>
	^ self
		onCountWithBehavior: [ :node | (node propertyAt: #haltCount) >= anInteger ]
]

{ #category : #private }
Halt class >> haltIfBlock: aBlock withCallingObjectFrom: haltSenderContext [

	(aBlock cull: haltSenderContext receiver)
		ifTrue: [self signalIn: haltSenderContext]
		ifFalse: [^ self].
]

{ #category : #private }
Halt class >> haltIfCallChain: haltSenderContext contains: aSelector [
	| cntxt |
	cntxt := haltSenderContext.
	[ cntxt isNil ] whileFalse: [ 
		cntxt selector = aSelector ifTrue: [ self signalIn: haltSenderContext ].
		cntxt := cntxt sender ]
]

{ #category : #halting }
Halt class >> if: condition [
	<debuggerCompleteToSender>
	"This is the typical message to use for inserting breakpoints during 
	debugging.
	
	The argument can be one of the following:
		- a block: if the Block has one arg, the calling object is bound to that.
		- an expression
 		- a selector: Halt if found in the call chain"
	LocalRecursionStopper during: [ 
		condition isSymbol ifTrue: [ 
			^ self haltIfCallChain: thisContext home sender contains: condition ].
		condition isBlock ifTrue: [ 
			^ self haltIfBlock: condition withCallingObjectFrom: thisContext home sender].
		condition ifTrue: [self signalIn: thisContext home sender ].
	]
]

{ #category : #halting }
Halt class >> ifNotTest [
	<debuggerCompleteToSender>
	"Halt if execution was not started by a test"
	CurrentExecutionEnvironment value isTest ifFalse: [self signalIn: thisContext home sender ].
]

{ #category : #halting }
Halt class >> ifTest [
	<debuggerCompleteToSender>
	"Halt if execution was started by a test"
	CurrentExecutionEnvironment value isTest ifTrue: [self signalIn: thisContext home sender ].
]

{ #category : #halting }
Halt class >> now [
	<debuggerCompleteToSender>
	self signal.
]

{ #category : #halting }
Halt class >> now: aString [
	"set a breakpoint with some explanation"
	<debuggerCompleteToSender>
	self signal: aString
]

{ #category : #halting }
Halt class >> onCount: anInteger [
	"Halts once when a count is reached"

	<debuggerCompleteToSender>
	^ self
		onCountWithBehavior: [ :node | (node propertyAt: #haltCount) = anInteger ]
]

{ #category : #halting }
Halt class >> onCountWithBehavior: behaviorBlock [
	"Halt on the anInteger-th time through, as defined in the behavior block passed as parameter.
	First parameter of the behaviorBlock is the AST node corresponding to the halt call."

	<debuggerCompleteToSender>
	| callingContext node |
	"if the compiler is not loaded, we fall back on the standard halt "
	self environment at: #OpalCompiler ifAbsent: [ ^ self now ].
	callingContext := thisContext sender sender sender.
	node := callingContext sourceNodeExecuted.
	(node hasProperty: #haltCount)
		ifFalse: [ node propertyAt: #haltCount put: 0 ].
	node propertyAt: #haltCount put: (node propertyAt: #haltCount) + 1.
	(behaviorBlock value: node)
		ifTrue: [ ^ self signal ]
]

{ #category : #halting }
Halt class >> once [
	"Stop once and only once the execution of the method containing the expression Halt once. If you need to get it stops another time after the first stop, use Halt resetOnce."

	| callingContext senderContext node |
	<debuggerCompleteToSender>
		
	"if the compiler is not loaded, we fall back on the standard halt "
	self environment at: #OpalCompiler ifAbsent: [ ^self now ].	
	
	callingContext := (thisContext findContextSuchThat: [ :ctx | 
		ctx method sendsAnySelectorOf: #(haltOnce) ]
	) ifNil: [ thisContext sender ].
	
	senderContext := thisContext.
	
	LocalRecursionStopper during: [
		"we need to ignore when the code below calls us again"
		node := callingContext sourceNodeExecuted.
		(node hasProperty: #Once) ifTrue: [ ^ self ]. 
		node propertyAt: #Once put: true.
		
		self signalIn: senderContext
		
	].
	
	

]

{ #category : #'once-reset' }
Halt class >> resetHaltOnCount [
	 #haltOnCount: senders do: [ :method | method ast removeProperty: #haltCount ifAbsent: [  ] ].
]

{ #category : #'once-reset' }
Halt class >> resetOnce [
	"Once stops an expression once and only once. Halt resetOnce will rearm and make sure that the execution of a method containing Halt once will stop another time."
	 (#haltOnce senders, #inspectOnce senders) do: [ :method | 
		method ast sendNodes do: [ :sendNode| 
			sendNode removeProperty: #Once ifAbsent: [  ]]]
]

{ #category : #private }
Halt >> completeProcess: aProcess with: aContext [
	^ aContext methodNode
		pragmaNamed: #debuggerCompleteToSender
		ifPresent: [ aProcess completeTo: aContext sender ]
		ifAbsent: [ super completeProcess: aProcess with: aContext ]
]

{ #category : #'priv handling' }
Halt >> defaultAction [
	"No one has handled this error, but now give them a chance to decide how to debug it.  If none handle this either then open debugger (see UnhandedError-defaultAction)"

	^ UIManager default unhandledErrorDefaultAction: self
	"^ UnhandledError signalForException: self" 
]

{ #category : #private }
Halt >> exception [
	"In the context management, it might happen that we get an Halt instead of an UnhandledError. The context will ask the exception. In case we have an halt, we should return self."

	^ self
]
