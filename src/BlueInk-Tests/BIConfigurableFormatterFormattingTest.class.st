"
Tests for BIConfigurableFormatter
"
Class {
	#name : #BIConfigurableFormatterFormattingTest,
	#superclass : #RBFormatterTest,
	#category : #'BlueInk-Tests'
}

{ #category : #testing }
BIConfigurableFormatterFormattingTest class >> shouldInheritSelectors [
	
	^ true
]

{ #category : #private }
BIConfigurableFormatterFormattingTest >> formatterClass [
	^ BIConfigurableFormatter
]
