"
Instance side #initialize method should be in 'initialization' protocol
"
Class {
	#name : 'ReInstanceSideInitializeMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideInitializeMethodProtocolRule class >> protocolName [

	^ 'initialization'
]

{ #category : 'accessing' }
ReInstanceSideInitializeMethodProtocolRule class >> selector [

	^ #initialize
]
