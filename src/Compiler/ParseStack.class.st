"
I keep track of the current and high position of the stack that will be needed by code being compiled.
"
Class {
	#name : #ParseStack,
	#superclass : #Object,
	#instVars : [
		'position',
		'length'
	],
	#category : 'Compiler-Support'
}

{ #category : #initialization }
ParseStack >> init [

	length := position := 0
]

{ #category : #accessing }
ParseStack >> pop: n [

	(position := position - n) < 0 
		ifTrue: [self error: 'Parse stack underflow']
]

{ #category : #results }
ParseStack >> position [

	^position
]

{ #category : #accessing }
ParseStack >> position: n [ 
	(position := n) > length
		ifTrue: [length := position]
]

{ #category : #printing }
ParseStack >> printOn: aStream [
	
	super printOn: aStream.
	aStream nextPutAll: ' at '; print: position; nextPutAll: ' of '; print: length
]

{ #category : #accessing }
ParseStack >> push: n [

	(position := position + n) > length 
		ifTrue: [length := position]
]

{ #category : #accessing }
ParseStack >> size [

	^length
]
