Class {
	#name : #ReProperMethodProtocolNameRuleForUtilities,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameRuleForUtilities >> protocolIdiom [ 

	^self use: 'utilities' insteadOf: #('utils' 'utility')
]
