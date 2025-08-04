"
This keeps track of the stack count for the BytecodeGenerator.
"
Class {
	#name : 'OCIRStackCount',
	#superclass : 'Object',
	#instVars : [
		'start',
		'position',
		'length'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'instance creation' }
OCIRStackCount class >> new [

	^ super new startAt: 0
]

{ #category : 'instance creation' }
OCIRStackCount class >> newOn: stack [

	^ self startAt: stack position
]

{ #category : 'instance creation' }
OCIRStackCount class >> startAt: pos [

	^ self new startAt: pos
]

{ #category : 'comparing' }
OCIRStackCount >> = other [

	^ self class == other class
	  and: [start = other start
	  and: [position = other position
	  and: [length = other size]]]
]

{ #category : 'comparing' }
OCIRStackCount >> hash [

	^ position hash bitXor: (length hash bitXor: start hash)
]

{ #category : 'results' }
OCIRStackCount >> length [

	^length
]

{ #category : 'results' }
OCIRStackCount >> linkTo: stackOrNil [

	stackOrNil ifNil: [^  self class newOn: self].
	^ self position = stackOrNil start
		ifTrue: [stackOrNil]
		ifFalse: [self error: 'stack out of sync in bytecode generator']
]

{ #category : 'affecting' }
OCIRStackCount >> pop [

	^ self pop: 1
]

{ #category : 'affecting' }
OCIRStackCount >> pop: n [

	(position := position - n) < 0
		ifTrue: [self error: 'stack underflow in bytecode generator']
]

{ #category : 'results' }
OCIRStackCount >> position [

	^position
]

{ #category : 'printing' }
OCIRStackCount >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' start '; print: start;
		nextPutAll: ' stop '; print: position;
		nextPutAll: ' max '; print: length
]

{ #category : 'affecting' }
OCIRStackCount >> push [
	^ self push: 1
]

{ #category : 'affecting' }
OCIRStackCount >> push: n [
	(position := position + n) > length
		ifTrue: [length := position]
]

{ #category : 'results' }
OCIRStackCount >> size [

	^length
]

{ #category : 'results' }
OCIRStackCount >> start [

	^ start
]

{ #category : 'initialize' }
OCIRStackCount >> startAt: pos [

	start := position := length := pos
]
