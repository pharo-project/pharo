"
Instance side #hash method should be in 'comparing' protocol
"
Class {
	#name : 'ReInstanceSideHashMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing - defaults' }
ReInstanceSideHashMethodProtocolRule >> protocolName [

	^ 'comparing'
]

{ #category : 'accessing - defaults' }
ReInstanceSideHashMethodProtocolRule >> selector [

	^ #hash
]
