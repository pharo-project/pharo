"
I'm a signal that captures the stack of execution.
"
Class {
	#name : 'StackSignal',
	#superclass : 'BeaconSignal',
	#instVars : [
		'stack'
	],
	#category : 'Beacon-Core-Signals',
	#package : 'Beacon-Core',
	#tag : 'Signals'
}

{ #category : 'actions' }
StackSignal class >> emit [ 

	^ self new emit; yourself
]

{ #category : 'accessing' }
StackSignal >> currentStack [
	
	| fullStack |
	stack := fullStack := thisContext copyStack.
	[ fullStack sender selector = #emit or: [ fullStack sender isNil ] ] 
		whileFalse: [ fullStack := fullStack sender ].
	fullStack ifNotNil: [ stack := fullStack.
							"we skip the emit and emit new emit"
							stack := stack sender sender sender. ].
	^ stack
]

{ #category : 'initialization' }
StackSignal >> prepareForDelivery [ 
	"Only reify the stack when a given logger is interested."
	
	self stack
]

{ #category : 'initialization' }
StackSignal >> stack [ 
	
	^ stack ifNil: [ stack := self currentStack ]
]
