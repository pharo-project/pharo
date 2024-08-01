"
Instance side #value method should be in 'evaluating' protocol
"
Class {
	#name : 'ReInstanceSideValueMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRule class >> protocolName [

	^ 'evaluating'
]

{ #category : 'accessing' }
ReInstanceSideValueMethodProtocolRule class >> selector [

	^ #value
]
