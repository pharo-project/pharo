"
I am group of methods marked by concrete tag
"
Class {
	#name : #ClyTaggedMethodGroup,
	#superclass : #ClyMethodGroup,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #testing }
ClyTaggedMethodGroup class >> isBasedOnMethodTag [
	^true
]

{ #category : #testing }
ClyTaggedMethodGroup class >> isEditableGroup [
	^true
]

{ #category : #'instance creation' }
ClyTaggedMethodGroup class >> withMethodsFrom: aClassScope taggedBy: tagName [
	^ClyTaggedMethodGroup named: tagName on: (ClyTaggedMethodsQuery by: tagName from: aClassScope)
]

{ #category : #operations }
ClyTaggedMethodGroup >> convertToMethodTag: aTagName [

	self renameMethodTagTo: aTagName
]

{ #category : #operations }
ClyTaggedMethodGroup >> importMethod: aMethod [
	super importMethod: aMethod.

	aMethod tagWith: self tag
]

{ #category : #operations }
ClyTaggedMethodGroup >> removeWithMethods [

	super removeWithMethods.
	methodQuery scope classesDo: [ :class | class removeProtocolIfEmpty: self tag ]
]

{ #category : #operations }
ClyTaggedMethodGroup >> renameMethodTagTo: newTag [
	newTag = self tag ifTrue: [ ^self ].

	self methods do: [ :method |
		method tagWith: newTag.
		method untagFrom: self tag].

	methodQuery scope classesDo:  [ :class |
		(class tagsForMethods includes: newTag)
			ifFalse: [ class addMethodTag: newTag ].
		class removeProtocolIfEmpty: self tag]
]

{ #category : #accessing }
ClyTaggedMethodGroup >> tag [
	^ methodQuery tag
]
