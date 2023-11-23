"
Instance side #baseline: method should be in 'baselines' protocol
"
Class {
	#name : 'ReInstanceSideBaselineMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing - defaults' }
ReInstanceSideBaselineMethodProtocolRule >> protocolName [

	^ 'baselines'
]

{ #category : 'accessing - defaults' }
ReInstanceSideBaselineMethodProtocolRule >> selector [

	^ #baseline:
]
