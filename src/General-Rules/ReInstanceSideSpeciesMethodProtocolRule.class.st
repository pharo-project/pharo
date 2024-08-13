"
Instance side #species method should be in 'private' protocol
"
Class {
	#name : 'ReInstanceSideSpeciesMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideSpeciesMethodProtocolRule class >> protocolName [

	^ 'private'
]

{ #category : 'accessing - defaults' }
ReInstanceSideSpeciesMethodProtocolRule class >> selector [

	^ #species
]
