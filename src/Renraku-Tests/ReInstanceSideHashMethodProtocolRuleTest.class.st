"
A ReInstanceSideHashMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideHashMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideHashMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRuleTest >> properProtocolName [

	^ 'comparing'
]

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRuleTest >> sampleMethod [

	^ 'hash
	^ 3+4'
]

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRuleTest >> selector [

	^ #hash
]
