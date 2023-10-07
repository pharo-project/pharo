"
A ReInstanceSideFinalizeMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideFinalizeMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideFinalizeMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideFinalizeMethodProtocolRuleTest >> properProtocolName [

	^ 'finalization'
]

{ #category : 'accessing' }
ReInstanceSideFinalizeMethodProtocolRuleTest >> sampleMethod [

	^ 'finalize
	^ self'
]

{ #category : 'accessing' }
ReInstanceSideFinalizeMethodProtocolRuleTest >> selector [ 

	^ #finalize
]
