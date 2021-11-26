"
Rule to favour 'adding/removing' method protocol over similar ones (like 'adding-removing' or other)
"
Class {
	#name : #ReProperMethodProtocolNameForAddingRemovinggRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameForAddingRemovinggRule class >> protocolIdiom [ 

	^self use: 'adding/removing' insteadOf: #('adding-removing')
]
