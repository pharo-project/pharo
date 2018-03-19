"
Rule to favour 'converting' method protocol over similar ones (like 'conversion' or other)
"
Class {
	#name : #ReProperMethodProtocolNameRuleForConverting,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameRuleForConverting >> protocolIdiom [ 

	^self use: 'converting' insteadOf: #('conversion' 'conversions')
]
