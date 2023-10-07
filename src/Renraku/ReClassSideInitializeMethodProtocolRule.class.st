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

{ #category : 'accessing - defaults' }
ReClassSideInitializeMethodProtocolRule >> protocolName [

	^ 'class initialization'
]

{ #category : 'accessing - defaults' }
ReClassSideInitializeMethodProtocolRule >> selector [

	^ #initialize
]
