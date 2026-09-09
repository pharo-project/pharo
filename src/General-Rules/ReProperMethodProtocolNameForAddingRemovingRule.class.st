"
Rule to favour 'adding-removing' method protocol over similar ones (like 'adding/removing' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForAddingRemovingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForAddingRemovingRule class >> protocolIdiom [

	^self use: #'adding-removing' insteadOf: #(#'adding/removing')
]
