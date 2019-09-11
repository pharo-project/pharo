"
I hold tests for UIThemeDecorator.
"
Class {
	#name : #SpUIThemeDecoratorTest,
	#superclass : #TestCase,
	#instVars : [
		'themeDecorator'
	],
	#category : #'Spec2-Morphic-Backend-Tests'
}

{ #category : #running }
SpUIThemeDecoratorTest >> setUp [
	super setUp.
	themeDecorator := SpUIThemeDecorator exampleDecoratorToMakePluggableButtonRed
]

{ #category : #tests }
SpUIThemeDecoratorTest >> testDoesNotUnderstand [
	| block |
	themeDecorator
		property: #foo returnsValueOf: 42.
		
	self assert: (themeDecorator foo) equals: 42.
	
	block := [ 42 ].
	themeDecorator
		property: #fooBlock returnsValueOf: block.
	"Returns the result of the block evaluation."
	self assert: (themeDecorator fooBlock) equals: 42.
	
	block := [ :x | x + 42 ].
	
	themeDecorator
		property: #fooBlock: returnsValueOf: block.
	"Returns the result of the block evaluation."
	self assert: (themeDecorator fooBlock: 3) equals: 45.
]

{ #category : #tests }
SpUIThemeDecoratorTest >> testHasProperty [
	self
		assert: (themeDecorator hasProperty: #buttonNormalFillStyleFor:);
		assert: (themeDecorator hasProperty: #buttonMouseOverFillStyleFor:);
		assert: (themeDecorator hasProperty: #buttonPressedFillStyleFor:);
		deny: (themeDecorator hasProperty: #controlButtonSelectedDisabledFillStyleFor:);
		deny: (themeDecorator hasProperty: #scrollbarMouseOverBorderStyleFor:);
		deny: (themeDecorator hasProperty: #scrollbarThumbCornerStyleIn:)
]

{ #category : #tests }
SpUIThemeDecoratorTest >> testPropertyReturnsValueOf [
	| block |
	themeDecorator
		property: #foo returnsValueOf: 42.
		
	self assert: (themeDecorator objectAtProperty: #foo) equals: 42.
	
	block := [ 42 ].
	
	themeDecorator
		property: #fooBlock returnsValueOf: block.
	"Returns the block, not its value."
	self assert: (themeDecorator objectAtProperty: #fooBlock) equals: block.
]
