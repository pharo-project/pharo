"
Instance side #species method should be in 'private' protocol
"
Class {
	#name : 'ReInstanceSideSpeciesMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Rules',
	#package : 'Renraku',
	#tag : 'Rules'
}

{ #category : 'accessing - defaults' }
ReInstanceSideSpeciesMethodProtocolRule >> protocolName [

	^ 'private'
]

{ #category : 'accessing - defaults' }
ReInstanceSideSpeciesMethodProtocolRule >> selector [

	^ #species
]
