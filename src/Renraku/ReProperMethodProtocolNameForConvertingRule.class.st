"
Rule to favour 'converting' method protocol over similar ones (like 'conversion' or other)
"
Class {
	#name : #ReProperMethodProtocolNameForConvertingRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameForConvertingRule >> protocolIdiom [ 

	^self use: 'converting' insteadOf: #('conversion' 'conversions')
]
