"
Instance side #printOn: method should be in 'printing' protocol
"
Class {
	#name : 'ReInstanceSidePrintOnMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRule class >> protocolName [

	^ 'printing'
]

{ #category : 'accessing' }
ReInstanceSidePrintOnMethodProtocolRule class >> selector [

	^ #printOn:
]
