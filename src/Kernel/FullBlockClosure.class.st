"
A FullBlockClosure is a closure that can be independent of any outerContext if desired.  It has its own method (currently reusing the startpc inst var) and its own receiver.  outerContext can be either a MethodContext/Context or nil.

Instance Variables
	outerContext 	<Context>
	(startpc) compiledBlock <CompiledBlock> for compatibility, this is startpc.
	numArgs 		<SmallInteger>
	receiver:		<Object>
"
Class {
	#name : #FullBlockClosure,
	#superclass : #BlockClosure,
	#type : #variable,
	#instVars : [
		'receiver'
	],
	#category : #'Kernel-Methods'
}

{ #category : #scanning }
FullBlockClosure >> abstractBytecodeMessagesDo: aBlock [
	^ self compiledBlock abstractBytecodeMessagesDo: aBlock
]

{ #category : #private }
FullBlockClosure >> asContextWithSender: aContext [
	"Inner private support method for evaluation.  Do not use unless you know what you're doing."

	^(Context newForMethod: self compiledBlock)
		setSender: aContext
		receiver: self receiver
		method: self compiledBlock
		closure: self
		startpc: self startpc;
		privRefresh
]

{ #category : #accessing }
FullBlockClosure >> compiledBlock [
	"To be able to inherit from BlockClosure"
	^ startpc
]

{ #category : #accessing }
FullBlockClosure >> compiledBlock: aCompiledMethod [
	"To be able to inherit from BlockClosure"
	startpc := aCompiledMethod
]

{ #category : #accessing }
FullBlockClosure >> endPC [
	^ self compiledBlock endPC
]

{ #category : #scanning }
FullBlockClosure >> hasMethodReturn [
	"Answer whether the receiver has a method-return ('^') in its code."
	^ self compiledBlock hasMethodReturn
]

{ #category : #accessing }
FullBlockClosure >> home [
	^ outerContext ifNotNil: [ outerContext home ]
]

{ #category : #accessing }
FullBlockClosure >> method [
	^ self compiledBlock
]

{ #category : #accessing }
FullBlockClosure >> numArgs: n [
	numArgs := n
]

{ #category : #accessing }
FullBlockClosure >> numTemps [
	^ self compiledBlock numTemps
]

{ #category : #accessing }
FullBlockClosure >> outerContext: ctxt [
	outerContext := ctxt
]

{ #category : #accessing }
FullBlockClosure >> receiver [
	^ receiver
]

{ #category : #accessing }
FullBlockClosure >> receiver: anObject [
	receiver := anObject
]

{ #category : #evaluating }
FullBlockClosure >> rfvalue [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 207>
	<metaLinkOptions: #( + optionDisabledLink)>
	| newContext |
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : #evaluating }
FullBlockClosure >> rfvalueNoContextSwitch [
	"same as valueNoContextSwitch, for recursion stopping metalinks"
	<primitive: 209>
	<metaLinkOptions: #( + optionDisabledLink)>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed
]

{ #category : #simulation }
FullBlockClosure >> simulateValueWithArguments: anArray caller: aContext [
	"Simulate the valueWithArguments: primitive. Fail if anArray is not an array of the right arity."
	| newContext |
	newContext := (Context newForMethod: self compiledBlock)
						setSender: aContext
						receiver: receiver
						method: self compiledBlock
						closure: self
						startpc: self compiledBlock initialPC.
	((newContext objectClass: anArray) ~~ Array
	 or: [numArgs ~= anArray size]) ifTrue:
		[^Context primitiveFailTokenFor: nil].
	newContext stackp: self compiledBlock numTemps.
	1 to: numArgs do:
		[:i| newContext at: i put: (anArray at: i)].
	1 to: self basicSize do:
		[:i| newContext at: i + numArgs put: (self at: i)].
	^newContext
]

{ #category : #accessing }
FullBlockClosure >> sourceNode [
	^ self compiledBlock sourceNodeInOuter
]

{ #category : #accessing }
FullBlockClosure >> startpc [
	^ self compiledBlock initialPC
]

{ #category : #evaluating }
FullBlockClosure >> value [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 207>
	| newContext |
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : #evaluating }
FullBlockClosure >> value: firstArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the argument and copied values to the activation
	 as its argument and copied temps. Primitive. Essential."
	<primitive: 207>
	| newContext |
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : #evaluating }
FullBlockClosure >> value: firstArg value: secondArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 207>
	| newContext |
	numArgs ~= 2 ifTrue:
		[self numArgsError: 2].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			newContext at: 2 put: secondArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : #evaluating }
FullBlockClosure >> value: firstArg value: secondArg value: thirdArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 207>
	| newContext |
	numArgs ~= 3 ifTrue:
		[self numArgsError: 3].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			newContext at: 2 put: secondArg.
			newContext at: 3 put: thirdArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : #evaluating }
FullBlockClosure >> value: firstArg value: secondArg value: thirdArg value: fourthArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 207>
	| newContext |
	numArgs ~= 4 ifTrue:
		[self numArgsError: 4].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			newContext at: 1 put: firstArg.
			newContext at: 2 put: secondArg.
			newContext at: 3 put: thirdArg.
			newContext at: 4 put: fourthArg.
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]

{ #category : #evaluating }
FullBlockClosure >> valueNoContextSwitch [
	"An exact copy of BlockClosure>>value except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 209>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed
]

{ #category : #evaluating }
FullBlockClosure >> valueNoContextSwitch: anArg [
	"An exact copy of BlockClosure>>value: except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 209>
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed
]

{ #category : #evaluating }
FullBlockClosure >> valueWithArguments: anArray [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments in an anArray and copied values to
	 the activation as its arguments and copied temps. Primitive. Essential."
	<primitive: 208>
	| newContext |
	numArgs ~= anArray size ifTrue:
		[self numArgsError: anArray size].
	false
		ifTrue: "Old code to simulate the closure value primitive on VMs that lack it."
			[newContext := self asContextWithSender: thisContext sender.
			1 to: numArgs do:
				[:i| newContext at: i put: (anArray at: i)].
			thisContext privSender: newContext]
		ifFalse: [self primitiveFailed]
]
