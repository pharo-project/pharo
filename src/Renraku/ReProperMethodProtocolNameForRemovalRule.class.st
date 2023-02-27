Class {
	#name : #ReProperMethodProtocolNameForRemovalRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #idioms }
ReProperMethodProtocolNameForRemovalRule class >> protocolIdiom [

	^self use: #'removing' insteadOf: #(#'remove' #'removal')
]
