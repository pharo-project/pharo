Class {
	#name : 'ReProperMethodProtocolNameForRemovalRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForRemovalRule class >> protocolIdiom [

	^self use: #'removing' insteadOf: #(#'remove' #'removal')
]
