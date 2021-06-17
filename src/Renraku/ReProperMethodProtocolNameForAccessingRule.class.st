"
Rule to favour 'accessing' method protocol over similar ones (like 'accessor' or other)
"
Class {
	#name : #ReProperMethodProtocolNameForAccessingRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameForAccessingRule >> protocolIdiom [ 

	^self use: 'accessing' insteadOf: #('accessor' 'accessors' 'acessing' 'acccessing' 'accesing' 'acesing')
]
