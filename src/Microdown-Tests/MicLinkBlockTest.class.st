Class {
	#name : #MicLinkBlockTest,
	#superclass : #MicElementTest,
	#instVars : [
		'splitterClass'
	],
	#category : #'Microdown-Tests-Parser'
}

{ #category : #running }
MicLinkBlockTest >> setUp [

	super setUp.
	parser := self parser. 
	splitterClass := MicInlineParser
]

{ #category : #tests }
MicLinkBlockTest >> testPrintOn [

	| link |
	link := (splitterClass new parse: '[caption](http://www.pharo.org)') first.
	self assert: link asString equals: '[caption](http://www.pharo.org)'
]

{ #category : #tests }
MicLinkBlockTest >> testUrl [

	| link |
	link := (splitterClass new parse: '[caption](http://www.pharo.org:8080)') first.
	self assert: link url printString equals: 'http://www.pharo.org:8080'
]

{ #category : #tests }
MicLinkBlockTest >> testedClass [ 
	^ MicHorizontalLineBlock
]
