Class {
	#name : #OCASTWhileFalseTranslatorTest,
	#superclass : #OCASTDoubleBlockTranslatorTest,
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #'building suites' }
OCASTWhileFalseTranslatorTest class >> testParameters [
	^ super testParameters *
		(ParametrizedTestMatrix new
			forSelector: #optimization addOptions: { #optionInlineWhile . #optionInlineNone })
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileFalseTranslatorTest >> testWhileFalseExecutesLeftBlock [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileFalse: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp := temp + 1. temp > 4 ]'
		withSecondBlock: '[ ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileFalseTranslatorTest >> testWhileFalseExecutesLeftBlockWithTemp [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileFalse: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ | intemp |
			intemp := temp + 1.
			temp := intemp.
			intemp > 4 ]'
		withSecondBlock: '[ ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileFalseTranslatorTest >> testWhileFalseExecutesRightBlock [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileFalse: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp > 4 ]'
		withSecondBlock: '[ temp := temp + 1 ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileFalseTranslatorTest >> testWhileFalseExecutesRightBlockWithTemp [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	{argument1} whileFalse: {argument2}.
	^ temp'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp > 4 ]'
		withSecondBlock: '[ | intemp |
			intemp := temp + 1.
			temp := intemp ]'.

	self assert: result equals: 5.
]

{ #category : #'testing - blocks - optimized' }
OCASTWhileFalseTranslatorTest >> testWhileFalseReturnsNil [
	| result |
	result := self
		testComposedExample: 'example
	{definition}.
	temp := 0.
	^ {argument1} whileFalse: {argument2}'
		withTemps: #( 'temp' )
		withFirstBlock: '[ temp := temp + 1. temp > 4 ]'
		withSecondBlock: '[ ]'.

	self assert: result equals: nil.
]
