"
Instance side #printOn: method should be in 'printing' protocol
"
Class {
	#name : 'ReInstanceSidePrintOnMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSidePrintOnMethodProtocolRule class >> protocolName [

	^ 'printing'
]

{ #category : 'accessing - defaults' }
ReInstanceSidePrintOnMethodProtocolRule class >> selector [

	^ #printOn:
]
