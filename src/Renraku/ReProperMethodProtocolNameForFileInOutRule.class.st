Class {
	#name : 'ReProperMethodProtocolNameForFileInOutRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForFileInOutRule class >> protocolIdiom [

	^self use: #'file in/out' insteadOf: #(#'filein/out' #'fileIn/Out' #'fileIn/out' #'filein/Out' #'file-in/out')
]
