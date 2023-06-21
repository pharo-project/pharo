"
I am group of unclassified methods. I include only methods without any tag
"
Class {
	#name : #ClyNoTagMethodGroup,
	#superclass : #ClyMethodGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #accessing }
ClyNoTagMethodGroup class >> groupName [

	^ Protocol unclassified
]

{ #category : #testing }
ClyNoTagMethodGroup class >> isBasedOnMethodTag [
	^true
]

{ #category : #accessing }
ClyNoTagMethodGroup class >> priority [
	^8
]

{ #category : #'instance creation' }
ClyNoTagMethodGroup class >> withMethodsFrom: aMethodQuery [

	^self named: self groupName on: aMethodQuery
]

{ #category : #operations }
ClyNoTagMethodGroup >> renameMethodTagTo: newProtocolName [

	self methods do: [ :method | method protocol: newProtocolName ]
]
