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

{ #category : #test }
EFTest >> testLookAheadCodeIsResetWhenChangingConfiguration [
	"This is a test to show that the formatter got a problem with a cache. When we execute two formatting passes one after the other and changed the context. Some nodes are not formatted with the new context but with the old. This is due to a cache used by the formatter."
	
	| prettyPrinterInstance context1 context2 astNode res1 res2 |
	context1 := EFContext new.
	context2 := EFContext new lineUpBlockBrackets: true.
	prettyPrinterInstance := EFFormatter new installNewContext: context1.
	astNode := RBParser parseExpression: 
	'1 to: 4 do: [ :i | 
		Transcript
			show: i;
			cr;
			cr]'.
	
	res1 := prettyPrinterInstance format: astNode.
	res2 := (prettyPrinterInstance installNewContext: context2) format: astNode.
	
	self deny: res1 equals: res2
]
