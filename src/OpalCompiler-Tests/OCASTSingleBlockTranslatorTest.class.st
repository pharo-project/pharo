Class {
	#name : #OCASTSingleBlockTranslatorTest,
	#superclass : #OCASTTranslatorTest,
	#instVars : [
		'extractBlockInTemp'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTSingleBlockTranslatorTest class >> isAbtract [

	^ self == OCASTSingleBlockTranslatorTest
]

{ #category : #'building suites' }
OCASTSingleBlockTranslatorTest class >> testParameters [

	^ super testParameters * 
		(ParametrizedTestMatrix new
			forSelector: #extractBlockInTemp addOptions: { true . false };
			yourself)
]

{ #category : #support }
OCASTSingleBlockTranslatorTest >> extractBlockInTemp [

	^ extractBlockInTemp	ifNil: [ extractBlockInTemp := false ]
]

{ #category : #accessing }
OCASTSingleBlockTranslatorTest >> extractBlockInTemp: aBoolean [ 
	extractBlockInTemp := aBoolean
]

{ #category : #support }
OCASTSingleBlockTranslatorTest >> testComposedExample: template withBlock: blockCode [

	^ self testComposedExample: template withBlock: blockCode withArguments: #()
]

{ #category : #support }
OCASTSingleBlockTranslatorTest >> testComposedExample: template withBlock: blockCode withArguments: arguments [
	
	^ self
		testComposedExample: template
		withTemps: #() 
		withBlock: blockCode
		withArguments: arguments
]

{ #category : #support }
OCASTSingleBlockTranslatorTest >> testComposedExample: template withTemps: temps withBlock: blockCode [

	^ self testComposedExample: template  withTemps: temps withBlock: blockCode withArguments: #()
]

{ #category : #support }
OCASTSingleBlockTranslatorTest >> testComposedExample: template  withTemps: temps withBlock: blockCode withArguments: arguments [
	| declaration argument source methodTemps |
	methodTemps := OrderedCollection withAll: temps.
	self extractBlockInTemp ifTrue: [ 
			methodTemps add: 'block'.
			declaration := 'block := ', blockCode.
			argument := 'block' ]
		ifFalse: [
			declaration := ''.
			argument := blockCode ].
	
	declaration := String streamContents: [ :stream |
		stream nextPut: $|.
		methodTemps do: [ :each | stream nextPutAll: each; space ].
		stream nextPut: $|.
		stream nextPutAll: declaration. ].
	source := template format: { 
		'definition' -> declaration.
		'argument' -> argument } asDictionary.
	^ self testSource: source withArguments: arguments
]
