"
Instance side #baseline: method should be in 'baselines' protocol
"
Class {
	#name : 'ReInstanceSideBaselineMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideBaselineMethodProtocolRule class >> protocolName [

	^ 'baselines'
]

{ #category : 'accessing - defaults' }
ReInstanceSideBaselineMethodProtocolRule class >> selector [

	^ #baseline:
]
