"
Rule to favour 'utilities' method protocol over similar ones (like 'utils' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForUtilitiesRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForUtilitiesRule class >> protocolIdiom [

	^self use: #'utilities' insteadOf: #(#'utils' #'utility')
]
