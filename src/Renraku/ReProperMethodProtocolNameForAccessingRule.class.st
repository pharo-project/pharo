"
Rule to favour 'accessing' method protocol over similar ones (like 'accessor' or other)
"
Class {
	#name : 'ReProperMethodProtocolNameForAccessingRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForAccessingRule class >> protocolIdiom [

	^self use: #accessing insteadOf: #(#'accessor' #'accessors' #'acessing' #'acccessing' #'accesing' #'acesing')
]
