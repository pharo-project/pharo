Class {
	#name : #OCASTWhileTrueTranslatorTest,
	#superclass : #OCASTDoubleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTWhileTrueTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineWhile . #optionInlineNone })
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileTrueTranslatorTest >> testWhileTrueExecutesLeftBlock [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileTrue: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp := temp + 1. temp < 5 ]'
		withSecondBlock: '[ ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileTrueTranslatorTest >> testWhileTrueExecutesLeftBlockWithTemp [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileTrue: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ | intemp |
			intemp := temp + 1.
			temp := intemp.
			intemp < 5 ]'
		withSecondBlock: '[ ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileTrueTranslatorTest >> testWhileTrueExecutesRightBlock [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileTrue: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp < 5 ]'
		withSecondBlock: '[ temp := temp + 1 ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileTrueTranslatorTest >> testWhileTrueExecutesRightBlockWithTemp [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileTrue: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp < 5 ]'
		withSecondBlock: '[ | intemp |
			intemp := temp + 1.
			temp := intemp.
			temp ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileTrueTranslatorTest >> testWhileTrueReturnsNil [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	^ {argument1} whileTrue: {argument2}'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp := temp + 1. temp < 5 ]'
		withSecondBlock: '[ ]'.

	self assert: result equals: nil.
]
