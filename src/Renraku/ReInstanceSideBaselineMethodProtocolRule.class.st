"
Instance side #baseline: method should be in 'baselines' protocol
"
Class {
	#name : 'ReInstanceSideBaselineMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRule class >> protocolName [

	^ 'baselines'
]

{ #category : 'accessing' }
ReInstanceSideBaselineMethodProtocolRule class >> selector [

	^ #baseline:
]
