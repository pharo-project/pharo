"
Class side #initialize method should be in 'class initialization' protocol
"
Class {
	#name : 'ReClassSideInitializeMethodProtocolRule',
	#superclass : 'ReProperClassMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRule class >> protocolName [

	^ 'class initialization'
]

{ #category : 'accessing' }
ReClassSideInitializeMethodProtocolRule class >> selector [

	^ #initialize
]
