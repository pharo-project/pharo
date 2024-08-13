"
Rule to favour 'removing' method protocol over similar ones (like 'remove', 'removal' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForRemovalRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForRemovalRule class >> protocolIdiom [

	^self use: #'removing' insteadOf: #(#'remove' #'removal')
]
