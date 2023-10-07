"
A ReInstanceSideSpeciesMethodProtocolRuleTest is a test class for testing the behavior of ReInstanceSideSpeciesMethodProtocolRule
"
Class {
	#name : 'ReInstanceSideSpeciesMethodProtocolRuleTest',
	#superclass : 'ReProperInstanceMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRuleTest >> properProtocolName [

	^ 'private'
]

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRuleTest >> sampleMethod [

	^ 'species
	
	^ self class'
]

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRuleTest >> selector [

	^ #species
]
