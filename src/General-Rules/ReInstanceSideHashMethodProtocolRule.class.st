"
Instance side #hash method should be in 'comparing' protocol
"
Class {
	#name : 'ReInstanceSideHashMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideHashMethodProtocolRule class >> protocolName [

	^ 'comparing'
]

{ #category : 'accessing - defaults' }
ReInstanceSideHashMethodProtocolRule class >> selector [

	^ #hash
]
