"
I am an optimized stream for writing and reading *indexes*, i.e. positive integers with a known upper bound. 
"
Class {
	#name : #FLIndexStream,
	#superclass : #Object,
	#instVars : [
		'stream',
		'digits'
	],
	#category : #'Fuel-Core-Streams'
}

{ #category : #'instance creation' }
FLIndexStream class >> on: aStream digits: aNumberOfDigits [

	^ self basicNew 
		initializeOn: aStream 
		digits: aNumberOfDigits;
		yourself.

	
]

{ #category : #'initialize-release' }
FLIndexStream >> initializeOn: aStream digits: aNumberOfDigits [

	self initialize.
	stream := aStream.
	digits := aNumberOfDigits
]

{ #category : #accessing }
FLIndexStream >> nextIndex [
	
	| s |
	s := 0.
	1 to: digits do: 
		[:i | s := (s bitShift: 8) bitOr: stream next].
	^ s
]

{ #category : #accessing }
FLIndexStream >> nextIndexPut: v [
	1 to: digits do: [ :i | stream nextPut: (v byteAt: digits + 1 - i) ]
]
