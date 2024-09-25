"
Instance side #initialize method should be in 'initialization' protocol
"
Class {
	#name : 'ReInstanceSideInitializeMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideInitializeMethodProtocolRule class >> protocolName [

	^ 'initialization'
]

{ #category : 'accessing - defaults' }
ReInstanceSideInitializeMethodProtocolRule class >> selector [

	^ #initialize
]
