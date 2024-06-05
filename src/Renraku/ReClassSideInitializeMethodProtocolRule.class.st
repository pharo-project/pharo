"
Class side #initialize method should be in 'class initialization' protocol
"
Class {
	#name : 'ReClassSideInitializeMethodProtocolRule',
	#superclass : 'ReProperClassMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRule class >> protocolName [

	^ 'class initialization'
]

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRule class >> selector [

	^ #initialize
]
