"
Instance side #= method should be in 'comparing' protocol
"
Class {
	#name : 'ReInstanceSideEqualsMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'General-Rules-Instance Method Protocol',
	#package : 'General-Rules',
	#tag : 'Instance Method Protocol'
}

{ #category : 'accessing - defaults' }
ReInstanceSideEqualsMethodProtocolRule class >> protocolName [

	^ 'comparing'
]

{ #category : 'accessing - defaults' }
ReInstanceSideEqualsMethodProtocolRule class >> selector [

	^ #=
]
