"
A ReInstanceSideInitializeMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideInitializeMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideInitializeMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideInitializeMethodProtocolRuleTest >> properProtocolName [

	^ 'initialization'
]

{ #category : 'accessing' }
ReInstanceSideInitializeMethodProtocolRuleTest >> sampleMethod [

	^ 'initialize
	
	super initialize.'
]

{ #category : 'accessing' }
ReInstanceSideInitializeMethodProtocolRuleTest >> selector [

	^ #initialize
]
