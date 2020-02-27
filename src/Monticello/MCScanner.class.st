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
	#category : #'Monticello-Chunk Format'
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
	"This would be slightly simpler using recursion but it risks a stack overflow 
	 for some packages on some platforms, so we implement it using a local stack."

	| stack |
	stack := Stack with: Stack new.
	[
		| c |
		stream skipSeparators.
		c := stream peek.
		"We essentially ignore the $# symbol and expect what follows to be a symbol or array"
		c = $# ifTrue: [ c := stream next; peek ].
		"Note that #'foo' will be treated as a String, not a Symbol"
		c = $' ifTrue: [ stack top push: self nextString ] ifFalse: [
		"Any alphanumeric, including an integer, is treated as a Symbol even if not preceeded by a $#"
		c isAlphaNumeric ifTrue: [ stack top push: self nextSymbol ] ifFalse: [
		"For an array, start a new level on the stack"
		c = $( ifTrue: [ stream next. stack push: Stack new ] ifFalse: [
		"At the end of an array, so add it to the previous level"
		c = $) ifTrue: [ | x | stream next. x := stack pop. stack top push: x asArray reverse ] ifFalse: [ 
		"Unexpected token"
		self error: 'Unknown token type' ]]]].
		"Keep looping while we are in an array"
		1 < stack size.
	] whileTrue: [].
	^stack top top
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
