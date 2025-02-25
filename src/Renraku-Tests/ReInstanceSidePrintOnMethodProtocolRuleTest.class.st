"
A ReInstanceSidePrintOnMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSidePrintOnMethodProtocolRule
"
Class {
	#name : 'ReInstanceSidePrintOnMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRuleTest >> properProtocolName [

	^ 'printing'
]

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRuleTest >> sampleMethod [

	^ 'printOn: aStream
	
	aStream nextPutAll: ''Hello'''
]

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRuleTest >> selector [

	^ #printOn:
]
