Class {
	#name : #OCASTDoubleBlockTranslatorTest,
	#superclass : #OCASTSingleBlockTranslatorTest,
	#instVars : [
		'extractBlock2InTemp'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #private }
OCASTDoubleBlockTranslatorTest class >> isAbstract [

	^ self == OCASTDoubleBlockTranslatorTest
]

{ #category : #'building suites' }
OCASTDoubleBlockTranslatorTest class >> testParameters [

	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #extractBlock2InTemp addOptions: { true . false };
			yourself)
]

{ #category : #accessing }
OCASTDoubleBlockTranslatorTest >> extractBlock2InTemp [
	^ extractBlock2InTemp
]

{ #category : #accessing }
OCASTDoubleBlockTranslatorTest >> extractBlock2InTemp: anObject [
	extractBlock2InTemp := anObject
]

{ #category : #tests }
OCASTDoubleBlockTranslatorTest >> testComposedExample: template withFirstBlock: firstBlockCode withSecondBlock: secondBlockCode [

	^ self
		testComposedExample: template
		withTemps: #()
		withFirstBlock: firstBlockCode
		withSecondBlock: secondBlockCode
]

{ #category : #tests }
OCASTDoubleBlockTranslatorTest >> testComposedExample: template withTemps: predefinedTemps withFirstBlock: firstBlockCode withSecondBlock: secondBlockCode [
	| temps declaration argument1 argument2 source |
	temps := OrderedCollection new.
	temps addAll: predefinedTemps.
	self extractBlockInTemp ifTrue: [ 
			temps add: 'block1'.
			declaration := 'block1 := ', firstBlockCode, '.'.
			argument1 := 'block1' ]
		ifFalse: [
			declaration := ''.
			argument1 := firstBlockCode ].
	self extractBlock2InTemp ifTrue: [ 
			temps add: 'block2'.
			declaration := declaration , 'block2 := ', secondBlockCode, '.'.
			argument2 := 'block2' ]
		ifFalse: [
			argument2 := secondBlockCode ].

	declaration := String streamContents: [ :stream |
		stream nextPut: $|.
		temps do: [ :each | stream nextPutAll: each; space ].
		stream nextPut: $|.
		stream nextPutAll: declaration.
	].

	source := template format: { 
		'definition' -> declaration.
		'argument1' -> argument1.
		'argument2' -> argument2 } asDictionary.
	^ self testSource: source.
]
