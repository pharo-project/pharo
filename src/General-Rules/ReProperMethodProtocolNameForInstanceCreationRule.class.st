"
Rule to favour 'instance creation' method protocol over similar ones (like 'instance-creation' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForInstanceCreationRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForInstanceCreationRule class >> protocolIdiom [

	^self use: #'instance creation' insteadOf: #(#'instance-creation' #'instances-creation' #'instances creation')
]
