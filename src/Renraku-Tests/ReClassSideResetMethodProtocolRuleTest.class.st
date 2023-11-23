Class {
	#name : 'ReClassSideResetMethodProtocolRuleTest',
	#superclass : 'ReProperClassMethodProtocolRuleTest',
	#category : 'Renraku-Tests-Rules',
	#package : 'Renraku-Tests',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRuleTest >> properProtocolName [

	^ 'class initialization'
]

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRuleTest >> sampleMethod [

	^ 'reset
	^ self'
]

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRuleTest >> selector [ 

	^ #reset
]
