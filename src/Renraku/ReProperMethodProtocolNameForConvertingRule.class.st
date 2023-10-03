"
Rule to favour 'converting' method protocol over similar ones (like 'conversion' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForConvertingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForConvertingRule class >> protocolIdiom [

	^self use: #'converting' insteadOf: #(#'conversion' #'conversions')
]
