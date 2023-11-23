"
A ReInstanceSideEqualsMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideEqualsMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideEqualsMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideEqualsMethodProtocolRuleTest >> properProtocolName [

	^ 'comparing'
]

{ #category : 'accessing' }
ReInstanceSideEqualsMethodProtocolRuleTest >> sampleMethod [

	^ '= anotherObject
	^ false'
]

{ #category : 'accessing' }
ReInstanceSideEqualsMethodProtocolRuleTest >> selector [

	^ #=
]
