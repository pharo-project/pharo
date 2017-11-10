"
This keeps track of the stack count for the BytecodeGenerator.
"
Class {
	#name : #IRStackCount,
	#superclass : #Object,
	#instVars : [
		'start',
		'position',
		'length'
	],
	#category : #OpalCompiler-Core-Bytecode
}

{ #category : #'instance creation' }
IRStackCount class >> new [

	^ super new startAt: 0
]

{ #category : #'instance creation' }
IRStackCount class >> newOn: stack [

	^ self startAt: stack position
]

{ #category : #'instance creation' }
IRStackCount class >> startAt: pos [

	^ self new startAt: pos
]

{ #category : #comparing }
IRStackCount >> = other [

	^ self class == other class 
	  and: [start = other start
	  and: [position = other position
	  and: [length = other size]]]
]

{ #category : #comparing }
IRStackCount >> hash [

	^ position hash bitXor: (length hash bitXor: start hash)
]

{ #category : #results }
IRStackCount >> length [

	^length
]

{ #category : #results }
IRStackCount >> linkTo: stackOrNil [

	stackOrNil ifNil: [^  self class newOn: self].
	^ self position = stackOrNil start
		ifTrue: [stackOrNil]
		ifFalse: [self error: 'stack out of sync in bytecode generator']
]

{ #category : #affecting }
IRStackCount >> pop [

	^ self pop: 1
]

{ #category : #affecting }
IRStackCount >> pop: n [

	(position := position - n) < 0 
		ifTrue: [self error: 'stack underflow in bytecode generator']
]

{ #category : #results }
IRStackCount >> position [

	^position
]

{ #category : #printing }
IRStackCount >> printOn: aStream [
	
	super printOn: aStream.
	aStream
		nextPutAll: ' start '; print: start;
		nextPutAll: ' stop '; print: position;
		nextPutAll: ' max '; print: length.

]

{ #category : #affecting }
IRStackCount >> push [
	^ self push: 1
]

{ #category : #affecting }
IRStackCount >> push: n [
	(position := position + n) > length 
		ifTrue: [length := position]
]

{ #category : #results }
IRStackCount >> size [

	^length
]

{ #category : #results }
IRStackCount >> start [

	^ start
]

{ #category : #initialize }
IRStackCount >> startAt: pos [

	start := position := length := pos
]
