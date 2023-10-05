"
A ReClassSideInitializeMethodProtocolRuleTest is a test class for testing the behavior of ReClassSideInitializeMethodProtocolRule
"
Class {
	#name : 'ReClassSideInitializeMethodProtocolRuleTest',
	#superclass : 'ReProperClassMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRuleTest >> properProtocolName [

	^ 'class initialization'
]

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRuleTest >> sampleMethod [

	^ 'initialize
	^ self'
]

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRuleTest >> selector [ 

	^ #initialize
]
