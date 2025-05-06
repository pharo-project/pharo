"
Halt is a special exception provided to support breakpoints as defined in  Object>>halt.

## Basic usage

To manually set a breakpoint you can use message halt as follows: 

```			
anObject halt. 
```
You can also use 

```
Halt now
```

```
Halt now: 'With a nice explanation'
```

## Advanced usage
The class Halt supports different halting mechanisms such as conditionals, based on iteration or simply stopping the first time. 


### Conditionals

```
foo
	...
	Halt if: #invokedFromThisSelector
```

This expression will only stop the execution  of method foo if this method is in the call chain (is invoked from ) message invokedFromThisSelector.

```
foo
      ...
      Halt if: aBlock		
```

The execution of foo will stop if aBlock returns true. 

### Counting
Sometimes we need to stop after a given number of iteration.

```
Halt count: 5
```



### Once
It is really useful sometimes to stop only and only one time

```
foo
	...
	Halt once. 
```

It will be stopped the first time. 
Then if you need to rearm it to stop another time you should use

```
Halt resetOnce			
```


### Implementation details

In the past,  `Halt` was a normal exception (not a kind of UnhandledException) and it was processed through UnhandledError indirection. 
But such design did not allow to debug the unhandled logic itself. For example halt in `UnhandledError>>#defaultAction` led to infinite recursion.


Note that `once`, `upTo:`, `fromCount:`, and `onCount:` are call site depend in the sense that you can have two in the same methods and each own 
will have its own ""memory"".

Certain messages over a API in Object and one in Halt. The pattern to implement them or new ones is always the same and described now taking `once` implementation as example. 

- There should be an internal method getting a context as argument. 
- The method in Halt and object should be tagged with `<debuggerCompleteToSender>` 
- Both APIs should call the internal method. 

``` 
Halt >> once
   <debuggerCompleteToSender>
	
	self once: thisContext sender
```

```
Halt >> haltOnce
	<debuggerCompleteToSender>
	
	Halt once: thisContext sender
```






"
Class {
	#name : 'Halt',
	#superclass : 'UnhandledException',
	#classVars : [
		'OnCountDictionary',
		'OnceDictionary'
	],
	#category : 'Debugging-Utils',
	#package : 'Debugging-Utils'
}

{ #category : 'testing' }
Halt class >> captureIfSignalledWhenStepping [
	^self == Halt
]

{ #category : 'on count' }
Halt class >> counterFor: method andPc: pc [

	| methodStructure value |
	methodStructure := OnCountDictionary at: method ifAbsent: [ nil ].
		methodStructure 
			ifNotNil: [ :dict | 
				| valueForPc |
				"we have a onCount for the method but we should check if this
				is the one for a given callsite.
				If this is not we add it, else just increase the counter."
				
				valueForPc := dict at:  pc ifAbsent: [ nil ].
				valueForPc 
					ifNil: [ dict at:  pc put: 1. 
						value := 1.]
					ifNotNil: [ :v | dict at:  pc put: v + 1.
						value := v + 1 ]]	
			ifNil: [ 
				"There was not entry in for the method at all, so we add it."
				OnCountDictionary at: method put: (Dictionary new at:  pc put: 1; yourself).
				value := 1. ].
		^ value
]

{ #category : 'testing' }
Halt class >> during: aBlock [

	| onceTable counTable |
	onceTable := OnceDictionary.
	counTable := OnCountDictionary.
	aBlock ensure: 
		[ OnceDictionary := onceTable.
		OnCountDictionary := counTable ]
]

{ #category : 'halting' }
Halt class >> fromCount: anInteger [
	"Always halt after a count has been reached"

	<debuggerCompleteToSender>
	^ self onCountContext: thisContext sender haltIfPredicate:  [ :v | v >= anInteger ]
]

{ #category : 'private' }
Halt class >> haltIfBlock: aBlock withCallingObjectFrom: haltSenderContext [

	(aBlock cull: haltSenderContext receiver)
		ifTrue: [self signalIn: haltSenderContext]
		ifFalse: [^ self]
]

{ #category : 'private' }
Halt class >> haltIfCallChain: haltSenderContext contains: aSelector [
	| cntxt |
	cntxt := haltSenderContext.
	[ cntxt isNil ] whileFalse: [
		cntxt selector = aSelector ifTrue: [ self signalIn: haltSenderContext ].
		cntxt := cntxt sender ]
]

{ #category : 'halting' }
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

{ #category : 'halting' }
Halt class >> ifNotTest [
	<debuggerCompleteToSender>
	"Halt if execution was not started by a test"
	CurrentExecutionEnvironment value isTest ifFalse: [self signalIn: thisContext home sender ]
]

{ #category : 'halting' }
Halt class >> ifTest [
	<debuggerCompleteToSender>
	"Halt if execution was started by a test"
	CurrentExecutionEnvironment value isTest ifTrue: [self signalIn: thisContext home sender ]
]

{ #category : 'class initialization' }
Halt class >> initialize [ 
	"self initialize"
	
	self initializeOnce. 
	self initializeOnCount.
]

{ #category : 'class initialization' }
Halt class >> initializeOnCount [ 

	OnCountDictionary := WeakIdentityKeyDictionary new
]

{ #category : 'class initialization' }
Halt class >> initializeOnce [
	"self initialize"
	OnceDictionary := WeakIdentityKeyDictionary new.
	
]

{ #category : 'halting' }
Halt class >> now [
	<debuggerCompleteToSender>
	self signal
]

{ #category : 'halting' }
Halt class >> now: aString [
	"set a breakpoint with some explanation"
	<debuggerCompleteToSender>
	self signal: aString
]

{ #category : 'halting' }
Halt class >> onCount: anInteger [
	"Halts once when a count is reached"

	<debuggerCompleteToSender>
	^ self onCountContext: thisContext sender haltIfPredicate:  [ :v | v = anInteger ]
]

{ #category : 'on count' }
Halt class >> onCountContext: callingContext haltIfPredicate:  aBlock [

	<debuggerCompleteToSender>
	| value method |
	
	method := callingContext method.
	
	LocalRecursionStopper during: [
		value := self counterFor: method andPc: callingContext pc.
	].
	(aBlock value: value)
		ifTrue: [ 
			^ self signal ] 
]

{ #category : 'halting' }
Halt class >> once [
	"Stop once and only once the execution of the method containing the expression Halt once. If you need to get it stops another time after the first stop, use Halt resetOnce."
	<debuggerCompleteToSender>
	
	self once: thisContext sender
]

{ #category : 'halting' }
Halt class >> once: callingContext [
	"Stop once and only once the execution of the method containing the expression Halt once. If you need to get it stops another time after the first stop, use Halt resetOnce."

	| senderContext method |
	<debuggerCompleteToSender>
	
	senderContext := thisContext.
	method := callingContext method.
	
	LocalRecursionStopper during: [
		(OnceDictionary at: method 
			ifPresent: [ :dict | 
				| vv |
				"we have a haltonce for the method but we should check if this
				is the one for a given callsite.
				If this is not we add it, else return to avoid firing it. In the future 
				we can have a haltOnce: 2 and decrement it"
				vv := dict at: callingContext pc ifAbsent: [ nil ].
				vv 
					ifNil: [ dict at: callingContext pc put: true ]
					ifNotNil: [ :v | ^ self ] ]
			ifAbsentPut: [ 
				"There was not entry in for the method at all, so we add it and fire."
				OnceDictionary at: method put: (Dictionary new at: callingContext pc put: true; yourself) ]
			).
		
		self signalIn: senderContext

	]
]

{ #category : 'once reset' }
Halt class >> resetHaltOnCount [
	 self initializeOnCount
]

{ #category : 'once reset' }
Halt class >> resetOnce [
	"Once stops an expression once and only once. Halt resetOnce will rearm and make sure that the execution of a method containing Halt once will stop another time."
	
	self initializeOnce
]

{ #category : 'halting' }
Halt class >> upTo: anInteger [
	"Always halt for a certain amount of times, then stops"
	<debuggerCompleteToSender>
	
	^ self onCountContext: thisContext sender haltIfPredicate:  [ :v | v <= anInteger ]
]

{ #category : 'private' }
Halt >> completeProcess: aProcess with: aContext [
	^ aContext methodNode
		pragmaNamed: #debuggerCompleteToSender
		ifPresent: [ aProcess completeTo: aContext sender ]
		ifAbsent: [ super completeProcess: aProcess with: aContext ]
]

{ #category : 'private' }
Halt >> exception [
	"In the context management, it might happen that we get an Halt instead of an UnhandledError. The context will ask the exception. In case we have an halt, we should return self."

	^ self
]
