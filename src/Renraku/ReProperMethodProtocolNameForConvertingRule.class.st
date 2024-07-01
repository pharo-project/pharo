"
Rule to favour 'converting' method protocol over similar ones (like 'conversion' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForConvertingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForConvertingRule class >> protocolIdiom [

	^self use: #'converting' insteadOf: #(#'conversion' #'conversions')
]
