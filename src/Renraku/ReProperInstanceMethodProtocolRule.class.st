"
Abstract superclass for rules checking if a specific instance method is properly categorized into a specific protocol
"
Class {
	#name : 'ReProperInstanceMethodProtocolRule',
	#superclass : 'ReProperMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'testing' }
ReProperInstanceMethodProtocolRule class >> checksClassMethod [

	^ false
]

{ #category : 'testing' }
ReProperInstanceMethodProtocolRule class >> isAbstract [

	^ self == ReProperInstanceMethodProtocolRule
]
