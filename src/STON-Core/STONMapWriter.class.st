"
STONDictionaryWriter helps in writing dictionary based STON representations.
"
Class {
	#name : 'STONMapWriter',
	#superclass : 'STONStreamWriter',
	#category : 'STON-Core-Writer',
	#package : 'STON-Core',
	#tag : 'Writer'
}

{ #category : 'accessing' }
STONMapWriter >> at: key put: value [
	first ifTrue: [ first := false ] ifFalse: [ writer mapElementSeparator ].
	writer encodeKey: key value: value
]
