"
Instance side #finalize method should be in 'finalization' protocol
"
Class {
	#name : 'ReInstanceSideFinalizeMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideFinalizeMethodProtocolRule class >> protocolName [

	^ 'finalization'
]

{ #category : 'accessing' }
ReInstanceSideFinalizeMethodProtocolRule class >> selector [

	^ #finalize
]
