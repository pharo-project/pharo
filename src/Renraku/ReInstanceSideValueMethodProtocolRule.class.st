"
Instance side #value method should be in 'evaluating' protocol
"
Class {
	#name : 'ReInstanceSideValueMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRule class >> protocolName [

	^ 'evaluating'
]

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRule class >> selector [

	^ #value
]
