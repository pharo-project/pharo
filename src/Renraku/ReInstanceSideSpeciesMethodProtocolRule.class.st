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

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRule class >> protocolName [

	^ 'private'
]

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRule class >> selector [

	^ #species
]
