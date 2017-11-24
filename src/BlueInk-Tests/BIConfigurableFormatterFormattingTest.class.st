Class {
	#name : #BIConfigurableFormatterFormattingTest,
	#superclass : #RBFormatterTest,
	#category : #'BlueInk-Tests'
}

{ #category : #testing }
BIConfigurableFormatterFormattingTest class >> shouldInheritSelectors [
	
	^ true
]

{ #category : #default }
BIConfigurableFormatterFormattingTest >> formatterClass [
	^ BIConfigurableFormatter
]
