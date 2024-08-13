"
Rule to favour 'instance creation' method protocol over similar ones (like 'instance-creation' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForInstanceCreationRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForInstanceCreationRule class >> protocolIdiom [

	^self use: #'instance creation' insteadOf: #(#'instance-creation' #'instances-creation' #'instances creation')
]
