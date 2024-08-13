"
Instance side #species method should be in 'private' protocol
"
Class {
	#name : 'ReInstanceSideSpeciesMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRule class >> protocolName [

	^ 'private'
]

{ #category : 'accessing' }
ReInstanceSideSpeciesMethodProtocolRule class >> selector [

	^ #species
]
