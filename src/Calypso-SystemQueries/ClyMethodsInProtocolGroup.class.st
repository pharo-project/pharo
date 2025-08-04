"
I am group of methods in a specific protocol
"
Class {
	#name : 'ClyMethodsInProtocolGroup',
	#superclass : 'ClyMethodGroup',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'testing' }
ClyMethodsInProtocolGroup class >> isBasedOnProtocol [
	^true
]

{ #category : 'testing' }
ClyMethodsInProtocolGroup class >> isEditableGroup [
	^true
]

{ #category : 'instance creation' }
ClyMethodsInProtocolGroup class >> withMethodsFrom: aClassScope inProtocol: protocolName [

	^ self named: protocolName on: (ClyMethodsInProtocolQuery by: protocolName from: aClassScope)
]

{ #category : 'operations' }
ClyMethodsInProtocolGroup >> categorizeMethodsIn: newProtocolName [

	newProtocolName = self protocol ifTrue: [ ^ self ].

	super categorizeMethodsIn: newProtocolName
]

{ #category : 'operations' }
ClyMethodsInProtocolGroup >> importMethod: aMethod [

	super importMethod: aMethod.

	aMethod protocol: self protocol
]

{ #category : 'accessing' }
ClyMethodsInProtocolGroup >> protocol [

	^ methodQuery protocol
]

{ #category : 'operations' }
ClyMethodsInProtocolGroup >> removeProtocolFrom: aCollectionOfClasses [

	aCollectionOfClasses do: [ :class | class removeProtocol: self protocol ]
]
