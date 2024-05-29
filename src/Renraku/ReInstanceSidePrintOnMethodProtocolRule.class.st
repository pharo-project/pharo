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

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRule class >> protocolName [

	^ 'printing'
]

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRule class >> selector [

	^ #printOn:
]
