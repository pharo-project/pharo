"
A ReInstanceSideBaselineMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideBaselineMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideBaselineMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRuleTest >> properProtocolName [

	^ 'baselines'
]

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRuleTest >> sampleMethod [

	^ 'baseline: spec
	^ self'
]

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRuleTest >> selector [

	^ #baseline:
]
