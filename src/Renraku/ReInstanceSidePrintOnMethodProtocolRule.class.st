"
Instance side #printOn: method should be in 'printing' protocol
"
Class {
	#name : 'ReInstanceSidePrintOnMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing - defaults' }
ReInstanceSidePrintOnMethodProtocolRule >> protocolName [

	^ 'printing'
]

{ #category : 'accessing - defaults' }
ReInstanceSidePrintOnMethodProtocolRule >> selector [

	^ #printOn:
]
