"
Rule to favour 'utilities' method protocol over similar ones (like 'utils' or other)
"
Class {
	#name : #ReProperMethodProtocolNameForUtilitiesRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameForUtilitiesRule >> protocolIdiom [ 

	^self use: 'utilities' insteadOf: #('utils' 'utility')
]
