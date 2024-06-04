"
Instance side #= method should be in 'comparing' protocol
"
Class {
	#name : 'ReInstanceSideEqualsMethodProtocolRule',
	#superclass : 'ReProperInstanceMethodProtocolRule',
	#category : 'Renraku-Clean Code',
	#package : 'Renraku',
	#tag : 'Clean Code'
}

{ #category : 'accessing' }
ReInstanceSideEqualsMethodProtocolRule class >> protocolName [

	^ 'comparing'
]

{ #category : 'accessing' }
ReInstanceSideEqualsMethodProtocolRule class >> selector [

	^ #=
]
