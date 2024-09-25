"
Class side #reset method should be in 'class initialization' protocol
"
Class {
	#name : 'ReClassSideResetMethodProtocolRule',
	#superclass : 'ReProperClassMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRule class >> protocolName [

	^ 'class initialization'
]

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRule class >> selector [

	^ #reset
]
