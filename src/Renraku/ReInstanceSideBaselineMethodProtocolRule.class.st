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

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRule class >> protocolName [

	^ 'baselines'
]

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRule class >> selector [

	^ #baseline:
]
