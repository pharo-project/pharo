"
Tests for BIConfigurableFormatter
"
Class {
	#name : #BIConfigurableFormatterFormattingTest,
	#superclass : #RBFormatterTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #testing }
BIConfigurableFormatterFormattingTest class >> shouldInheritSelectors [
	
	^ true
]

{ #category : #private }
BIConfigurableFormatterFormattingTest >> formatterClass [
	^ BIConfigurableFormatter
]

{ #category : #tests }
BIConfigurableFormatterFormattingTest >> testIsPrettyPrinter [
	self assert: BIConfigurableFormatter isPrettyPrinter 
]

{ #category : #tests }
BIConfigurableFormatterFormattingTest >> testShouldReinitializeCodeStreamBetweenFormating [
	| astNode formater context |
	context := BIPrettyPrinterContext new.
	formater := BIConfigurableFormatter new installNewContext: context.
	astNode := self parserClass parseExpression: '1'.
	self assert: (formater format: astNode) equals: '1'
]
