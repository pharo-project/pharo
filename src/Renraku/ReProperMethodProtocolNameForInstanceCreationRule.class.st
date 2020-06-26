"
Rule to favour 'instance creation' method protocol over similar ones (like 'instance-creation' or other)
"
Class {
	#name : #ReProperMethodProtocolNameForInstanceCreationRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameForInstanceCreationRule >> protocolIdiom [ 

	^self use: 'instance creation' insteadOf: #('instance-creation' 'instances-creation' 'instances creation')
]
