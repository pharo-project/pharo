"
Rule to favour 'adding-removing' method protocol over similar ones (like 'adding/removing' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForAddingRemovingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForAddingRemovingRule class >> protocolIdiom [

	^self use: #'adding-removing' insteadOf: #(#'adding/removing')
]
