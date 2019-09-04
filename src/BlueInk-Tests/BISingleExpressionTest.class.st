Class {
	#name : #BISingleExpressionTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'configurationSelector',
		'formatterClass',
		'contextClass'
	],
	#category : #'BlueInk-Tests-Base'
}

{ #category : #'building suites' }
BISingleExpressionTest class >> testParameters [

	^ ParametrizedTestMatrix new
			addCase: { #formatterClass -> BIConfigurableFormatter . #contextClass -> BIPrettyPrinterContext };
		yourself.
]

{ #category : #hooks }
BISingleExpressionTest >> contextClass [
	^ contextClass
]

{ #category : #hooks }
BISingleExpressionTest >> contextClass: aContextClass [
	contextClass := aContextClass
]

{ #category : #hooks }
BISingleExpressionTest >> formatExpression: anExpression [

	| expr |
	expr := self parseExpression: anExpression.
	^ self formatter format: expr
	
]

{ #category : #hooks }
BISingleExpressionTest >> formatter [
	
	^ self formatterClass new
		installNewContext: (self perform: configurationSelector)
		yourself
]

{ #category : #hooks }
BISingleExpressionTest >> formatterClass [
	
	^ formatterClass
]

{ #category : #hooks }
BISingleExpressionTest >> formatterClass: aFormatterClass [
	formatterClass := aFormatterClass
]

{ #category : #hooks }
BISingleExpressionTest >> parseExpression: anExpression [
	^ self parserClass parseExpression: anExpression
]

{ #category : #hooks }
BISingleExpressionTest >> parserClass [
	^ RBParser
]
