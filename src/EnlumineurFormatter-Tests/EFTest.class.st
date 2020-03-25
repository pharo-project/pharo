"
There is a big warning about the design of these tests. 
The context is not initialized so that we are sure about the configuration we are building. 

"
Class {
	#name : #EFTest,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'configurationSelector',
		'formatterClass',
		'contextClass'
	],
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #'building suites' }
EFTest class >> testParameters [

	^ ParametrizedTestMatrix new
			addCase: { #formatterClass -> EFFormatter. #contextClass -> EFContext };
		yourself.
]

{ #category : #accessing }
EFTest >> contextClass [
	^ contextClass
]

{ #category : #accessing }
EFTest >> contextClass: anObject [
	contextClass := anObject
]

{ #category : #hooks }
EFTest >> formatExpression: anExpression [

	| expr |
	expr := self parseExpression: anExpression.
	^ self formatter format: expr.
	
]

{ #category : #hooks }
EFTest >> formatter [
	^ self formatterClass new
		installNewContext: (self perform: configurationSelector)
		yourself
]

{ #category : #hooks }
EFTest >> formatterClass [

	^ formatterClass
]

{ #category : #hooks }
EFTest >> formatterClass: aFormatterClass [
	formatterClass := aFormatterClass
]

{ #category : #hooks }
EFTest >> parseExpression: anExpression [

	^ self parserClass parseExpression: anExpression
]

{ #category : #hooks }
EFTest >> parserClass [
	^ RBParser
]
