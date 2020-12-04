"
I am an internal link pointing to a structure within the current kernel. I'm syntactically represented by `*@`, label, and `*` 

```
# Header1
@secheader1
	
A reference to header *@secheader1*
```

I reference my owning structure to be able to lookup the referenced structure. The referenced structure is cached in the instance variable `target`.
"
Class {
	#name : #PRInternalLink,
	#superclass : #PRLink,
	#instVars : [
		'target',
		'anchor'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #testing }
PRInternalLink class >> isAbstract [
	^ false
]

{ #category : #testing }
PRInternalLink class >> isValidReference: aString [
	^ true
]

{ #category : #accessing }
PRInternalLink class >> priority [
	^ 100
]

{ #category : #'instance creation' }
PRInternalLink class >> reference: aString [
	| index |
	^ (index := aString indexOf: $@) isZero
		ifTrue: [ super reference: aString ]
		ifFalse: [ 
			index = 1
				ifTrue: [ self new anchor: (aString copyFrom: index + 1 to: aString size) ]
				ifFalse: [ 
					self new
						reference: (aString copyFrom: 1 to: index - 1);
						anchor: (aString copyFrom: index + 1 to: aString size);
						yourself ] ]
]

{ #category : #comparing }
PRInternalLink >> = anObject [
	^ super = anObject
		and: [ self anchor = anObject anchor ]
]

{ #category : #visiting }
PRInternalLink >> accept: aVisitor [
	^ aVisitor visitInternalLink: self
]

{ #category : #accessing }
PRInternalLink >> anchor [
	"Answer an anchor string the receiver is pointing to."

	^ anchor
]

{ #category : #accessing }
PRInternalLink >> anchor: aString [
	anchor := aString trimBoth
]

{ #category : #testing }
PRInternalLink >> hasAnchor [
	^ self anchor isNotNil
]

{ #category : #testing }
PRInternalLink >> hasReference [
	^ self reference isNotNil
]

{ #category : #comparing }
PRInternalLink >> hash [
	^ super hash bitXor: self anchor hash
]

{ #category : #testing }
PRInternalLink >> isInternal [
	^ true
]

{ #category : #printing }
PRInternalLink >> printOn: aStream [

	super printOn: aStream.
	aStream space ; nextPutAll: '*@'; nextPutAll: anchor; nextPut: $*
]

{ #category : #accessing }
PRInternalLink >> renderAsText [
	| text |
	text := String new writeStream.
	self reference ifNotNil: [ :ref | text nextPutAll: ref ].
	self hasAnchor
		ifTrue: [ text
				nextPutAll: '@';
				nextPutAll: self anchor ].
	^ text contents
]
