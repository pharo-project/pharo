Class {
	#name : 'ReProperMethodProtocolNameForRemovalRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForRemovalRule class >> protocolIdiom [

	^self use: #'removing' insteadOf: #(#'remove' #'removal')
]
