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

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRule class >> protocolName [

	^ 'comparing'
]

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRule class >> selector [

	^ #hash
]
