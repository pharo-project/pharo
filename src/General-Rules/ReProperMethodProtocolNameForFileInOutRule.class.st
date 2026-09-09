"
Rule to favour 'file in/out' method protocol over similar ones (like 'fileIn/Out' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForFileInOutRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'General-Rules-Method Protocol Name',
	#package : 'General-Rules',
	#tag : 'Method Protocol Name'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForFileInOutRule class >> protocolIdiom [

	^self use: #'file in/out' insteadOf: #(#'filein/out' #'fileIn/Out' #'fileIn/out' #'filein/Out' #'file-in/out')
]
