"
Abstract superclass for rules checking if a specific class method is properly categorized into a specific protocol
"
Class {
	#name : 'ReProperClassMethodProtocolRule',
	#superclass : 'ReProperMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'testing' }
ReProperClassMethodProtocolRule class >> checksClassMethod [

	^ true
]

{ #category : 'testing' }
ReProperClassMethodProtocolRule class >> isAbstract [

	^ self == ReProperClassMethodProtocolRule
]
