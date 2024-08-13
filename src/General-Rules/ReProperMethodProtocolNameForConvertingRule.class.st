"
Rule to favour 'converting' method protocol over similar ones (like 'conversion' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForConvertingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForConvertingRule class >> protocolIdiom [

	^self use: #'converting' insteadOf: #(#'conversion' #'conversions')
]
