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

{ #category : 'accessing - defaults' }
ReInstanceSideValueMethodProtocolRule >> protocolName [

	^ 'evaluating'
]

{ #category : 'accessing - defaults' }
ReInstanceSideValueMethodProtocolRule >> selector [

	^ #value
]
