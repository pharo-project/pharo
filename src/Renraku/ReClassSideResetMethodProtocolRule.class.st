"
Class side #reset method should be in 'class initialization' protocol
"
Class {
	#name : 'ReClassSideResetMethodProtocolRule',
	#superclass : 'ReProperClassMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRule class >> protocolName [

	^ 'class initialization'
]

{ #category : 'accessing' }
ReClassSideResetMethodProtocolRule class >> selector [

	^ #reset
]
