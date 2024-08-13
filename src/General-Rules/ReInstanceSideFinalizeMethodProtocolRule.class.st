"
Instance side #finalize method should be in 'finalization' protocol
"
Class {
	#name : 'ReInstanceSideFinalizeMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideFinalizeMethodProtocolRule class >> protocolName [

	^ 'finalization'
]

{ #category : 'accessing - defaults' }
ReInstanceSideFinalizeMethodProtocolRule class >> selector [

	^ #finalize
]
