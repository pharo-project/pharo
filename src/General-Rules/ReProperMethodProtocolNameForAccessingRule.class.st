"
Rule to favour 'accessing' method protocol over similar ones (like 'accessor' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForAccessingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForAccessingRule class >> protocolIdiom [

	^self use: #accessing insteadOf: #(#'accessor' #'accessors' #'acessing' #'acccessing' #'accesing' #'acesing')
]
