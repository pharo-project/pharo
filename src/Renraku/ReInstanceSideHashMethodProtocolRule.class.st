"
Instance side #hash method should be in 'comparing' protocol
"
Class {
	#name : 'ReInstanceSideHashMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRule class >> protocolName [

	^ 'comparing'
]

{ #category : 'accessing' }
ReInstanceSideHashMethodProtocolRule class >> selector [

	^ #hash
]
