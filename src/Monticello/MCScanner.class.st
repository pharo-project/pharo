"
I scan / tokenize metadata  (package name, version info, ancestry, dependencies, ...) found in .mcz files.

For example, try:

MCScanner scan:  '(name ''MyPackage-ll.6'' message ''Fix bug xxx'' id ''b21dbd73-f1c3-2746-a3cc-92f1d4edea28'')' readStream
"
Class {
	#name : #MCScanner,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : 'Monticello-Chunk Format'
}

{ #category : #'public api' }
MCScanner class >> scan: aStream [
	^ (self new stream: aStream) next
]

{ #category : #'public api' }
MCScanner class >> scanTokens: aString [
	"compatibility"
	^ Array with: (self scan: aString readStream)
]

{ #category : #actions }
MCScanner >> next [
	| c |
	stream skipSeparators.
	c := stream peek.
	c = $# ifTrue: [c := stream next; peek].
	c = $' ifTrue: [^ self nextString].
	c = $( ifTrue: [^ self nextArray].
	c isAlphaNumeric ifTrue: [^ self nextSymbol].
	self error: 'Unknown token type'.	
]

{ #category : #actions }
MCScanner >> nextArray [
	stream next. "("
	^ Array streamContents: [:s |
		[	stream skipSeparators.
			(stream peek = $)) or: [stream atEnd]
		] whileFalse: [ s nextPut: self next ].
		stream next = $) 
			ifFalse: [ self error: 'Unclosed array' ]]
]

{ #category : #actions }
MCScanner >> nextString [
	^ stream nextDelimited: $'
]

{ #category : #actions }
MCScanner >> nextSymbol [
	^ (String streamContents:
		[:s |
		[stream peek isAlphaNumeric] whileTrue: [s nextPut: stream next]]) asSymbol
			
]

{ #category : #accessing }
MCScanner >> stream: aStream [
	stream := aStream
]
