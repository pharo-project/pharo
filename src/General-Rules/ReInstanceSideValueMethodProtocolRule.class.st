"
Instance side #value method should be in 'evaluating' protocol
"
Class {
	#name : 'ReInstanceSideValueMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideValueMethodProtocolRule class >> protocolName [

	^ 'evaluating'
]

{ #category : 'accessing - defaults' }
ReInstanceSideValueMethodProtocolRule class >> selector [

	^ #value
]
