"
A ReInstanceSideValueMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideValueMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideValueMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRuleTest >> properProtocolName [

	^ 'evaluating'
]

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRuleTest >> sampleMethod [

	^ 'value
	
	^ self'
]

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRuleTest >> selector [

	^ #value
]
