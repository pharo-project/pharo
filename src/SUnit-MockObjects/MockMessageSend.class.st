"
I am a message that a MockObject instance has been trained to expect.
"
Class {
	#name : 'MockMessageSend',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'arguments',
		'behavior'
	],
	#category : 'SUnit-MockObjects-Core',
	#package : 'SUnit-MockObjects',
	#tag : 'Core'
}

{ #category : 'instance creation' }
MockMessageSend class >> on: aSymbol with: anArray do: aBlock [

	^ self new
		  on: aSymbol with: anArray do: aBlock;
		  yourself
]

{ #category : 'testing' }
MockMessageSend >> matches: aMessage [

	^ (self matchesSelector: aMessage selector) and: [
		  self matchesArguments: aMessage arguments ]
]

{ #category : 'testing' }
MockMessageSend >> matchesArguments: anArray [
	"implementation note: the MockObject any acts as a joker to say that the object is counted but we do not care of its value. See the class comment of MockObject."
	
	arguments ifNil: [ ^ true ].
	arguments size = anArray size ifFalse: [ ^ false ].
	anArray with: arguments do: [ :actual :expected |
		(expected ~= MockObject any and: [ actual ~= expected ]) ifTrue: [
			^ false ] ].
	^ true
]

{ #category : 'testing' }
MockMessageSend >> matchesSelector: aSymbol [

	^ selector = aSymbol
]

{ #category : 'initialization' }
MockMessageSend >> on: aSymbol with: anArray do: aBlock [

	selector := aSymbol.
	arguments := anArray.
	behavior := aBlock
]

{ #category : 'printing' }
MockMessageSend >> printOn: aStream [

	aStream nextPutAll: selector.
	
	arguments isEmptyOrNil ifFalse: [
		arguments size > 1 ifTrue: [ aStream nextPutAll: ' (' ].
		aStream space.
		arguments
			do: [ :arg | arg printOn: aStream ]
			separatedBy: [ aStream nextPutAll: ', ' ].
		arguments size > 1 ifTrue: [ aStream nextPutAll: ' )' ]]
]

{ #category : 'accessing' }
MockMessageSend >> selector [

	^ selector
]

{ #category : 'evaluating' }
MockMessageSend >> valueWithPossibleArgs: anArray [

	^ behavior valueWithPossibleArgs: anArray
]
