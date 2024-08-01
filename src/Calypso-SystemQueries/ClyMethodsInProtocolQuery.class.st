"
I return methods which are particular protocol.

To instantiate my instances use following expressions: 

```st
	ClyMethodsInProtocolQuery by: #accessing.
	ClyMethodsInProtocolQuery by: #accessing from: aScope
```
 
Internal Representation and Key Implementation Points.

    Instance Variables
	protocol:		<Symbol>
"
Class {
	#name : 'ClyMethodsInProtocolQuery',
	#superclass : 'ClyMethodQuery',
	#instVars : [
		'protocol'
	],
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'instance creation' }
ClyMethodsInProtocolQuery class >> by: protocol [

	^ self new
		  protocol: protocol;
		  yourself
]

{ #category : 'instance creation' }
ClyMethodsInProtocolQuery class >> by: protocol from: aScope [

	^ (self from: aScope)
		  protocol: protocol;
		  yourself
]

{ #category : 'comparing' }
ClyMethodsInProtocolQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ protocol = anObject protocol
]

{ #category : 'printing' }
ClyMethodsInProtocolQuery >> description [

	^ 'methods in protocol ' , protocol
]

{ #category : 'comparing' }
ClyMethodsInProtocolQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: protocol hash
]

{ #category : 'system changes' }
ClyMethodsInProtocolQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^ (aSystemAnnouncement affectsMethodsInProtocol: protocol) and: [ scope includesMethodsAffectedBy: aSystemAnnouncement ]
]

{ #category : 'printing' }
ClyMethodsInProtocolQuery >> printExtraInfoOn: aStream [

	super printExtraInfoOn: aStream.

	^ aStream nextPutAll: protocol
]

{ #category : 'accessing' }
ClyMethodsInProtocolQuery >> protocol [

	^ protocol
]

{ #category : 'accessing' }
ClyMethodsInProtocolQuery >> protocol: protocolName [

	protocol := protocolName
]

{ #category : 'testing' }
ClyMethodsInProtocolQuery >> selectsMethod: aMethod [

	^ aMethod protocolName == protocol
]
