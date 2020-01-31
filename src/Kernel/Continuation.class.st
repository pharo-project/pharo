"
I permit to save the execution flow and to restart it later. I was originally used in seaside.

Example :

You have an object with the instance variable executionFlow.

You save the current execution flow with :
Continuation currentDo: [ :cc | executionFlow := cc]

You restart the execution flow with :
executionFlow value: true



"
Class {
	#name : #Continuation,
	#superclass : #Object,
	#instVars : [
		'values'
	],
	#category : #'Kernel-Methods'
}

{ #category : #'instance creation' }
Continuation class >> current [
	^ self fromContext: thisContext sender
]

{ #category : #'instance creation' }
Continuation class >> currentDo: aBlock [
	^ aBlock value: (self fromContext: thisContext sender)
]

{ #category : #'instance creation' }
Continuation class >> fromContext: aStack [
	^self new initializeFromContext: aStack
]

{ #category : #'instance creation' }
Continuation class >> try: tryBlock or: orBlock or: orrBlock otherwise: elseBlock [
	^ self
		try: tryBlock
		otherwise: [ :res | 
			self
				try: [ :cont | orBlock cull: cont cull: res ]
				or: orrBlock
				otherwise: [ :rr :r | elseBlock cull: rr cull: r cull: res ] ]
]

{ #category : #'instance creation' }
Continuation class >> try: tryBlock or: orBlock otherwise: elseBlock [
	"	^ self
		currentDo: [ :success | 
			| localResult localResult1 |
			localResult := self
				currentDo: [ :local | success value: (tryBlock value: local) ].
			localResult1 := self
				currentDo: [ :local | success value: (orBlock cull: local cull: localResult) ].
			elseBlock cull: localResult1 cull: localResult ]"

	^ self
		try: tryBlock
		otherwise: [ :res | 
			self
				try: [ :cont | orBlock cull: cont cull: res ]
				otherwise: [ :lastRes | elseBlock cull: lastRes cull: res ] ]
]

{ #category : #'instance creation' }
Continuation class >> try: tryBlock otherwise: elseBlock [
	^ self
		currentDo: [ :success | 
			| localResult |
			localResult := self
				currentDo: [ :local | success value: (tryBlock value: local) ].
			elseBlock cull: localResult ]
]

{ #category : #private }
Continuation >> initializeFromContext: aContext [
	| valueStream context |
	valueStream := WriteStream on: (Array new: 20).
	context := aContext.
	[context notNil] whileTrue:
		[valueStream nextPut: context.
		1 to: context class instSize do: [:i | valueStream nextPut: (context instVarAt: i)].
		1 to: context size do: [:i | valueStream nextPut: (context at: i)].
		context := context sender].
	values := valueStream contents
]

{ #category : #accessing }
Continuation >> numArgs [
	^ 1
]

{ #category : #private }
Continuation >> restoreValues [
	| valueStream context |
	valueStream := values readStream.
	[valueStream atEnd] whileFalse:
		[context := valueStream next.
		1 to: context class instSize do: [:i | context instVarAt: i put: valueStream next].
		1 to: context size do: [:i | context at: i put: valueStream next]]
]

{ #category : #private }
Continuation >> terminate: aContext [
	| context |
	context := aContext.
	[context notNil] whileTrue: [context := context swapSender: nil]
]

{ #category : #evaluating }
Continuation >> value [
	self value: nil
]

{ #category : #evaluating }
Continuation >> value: anObject [
	"Invoke the continuation and answer anObject as return value."

	self terminate: thisContext.
	self restoreValues.
	thisContext swapSender: values first.
	^ anObject
]

{ #category : #evaluating }
Continuation >> valueWithArguments: anArray [
	anArray size = 1
		ifFalse: [ ^ self error: 'continuations can only be resumed with one argument' ].
	self value: anArray first
]
