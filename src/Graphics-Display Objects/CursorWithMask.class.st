"
A Cursor which additionally has a 16x16 transparency bitmap called a ""mask"".  See the comment of beCursorWithMask: for details on how the mask is treated.
"
Class {
	#name : #CursorWithMask,
	#superclass : #Cursor,
	#instVars : [
		'maskForm'
	],
	#category : 'Graphics-Display Objects'
}

{ #category : #'instance creation' }
CursorWithMask class >> derivedFrom: aForm [      "Cursor initNormalWithMask.  Cursor normal show"
	"aForm is presumably a cursor"
	| cursor mask ext |
	ext := aForm extent.
	cursor := self extent: ext.
	cursor copy: (1@1 extent: ext) from: 0@0 in: aForm rule: Form over.
	mask := Form extent: ext.
	(1@1) eightNeighbors do:
		[:p | mask copy: (p extent: ext) from: 0@0 in: aForm rule: Form under].
	cursor setMaskForm: mask.
	cursor offset: ((aForm offset - (1@1)) max: ext negated).
	^ cursor
]

{ #category : #converting }
CursorWithMask >> asCursorForm [
	| form |
	form := Form extent: self extent depth: 8.
	form fillShape: maskForm fillColor: Color white.
	form fillShape: self fillColor: Color black at: offset negated.
	^ form offset: offset
]

{ #category : #primitives }
CursorWithMask >> beCursor [
	maskForm unhibernate.
	^ self beCursorWithMask: maskForm
]

{ #category : #mask }
CursorWithMask >> hasMask [
	^true
]

{ #category : #mask }
CursorWithMask >> maskForm [
	^ maskForm
]

{ #category : #mask }
CursorWithMask >> setMaskForm: aForm [
	maskForm := aForm
]

{ #category : #mask }
CursorWithMask >> storeOn: aStream base: anInteger [

	aStream nextPut: $(.
	super storeOn: aStream base: anInteger.
	aStream nextPutAll: ' setMaskForm: '.
	maskForm storeOn: aStream base: anInteger.
	aStream nextPut: $)
]

{ #category : #mask }
CursorWithMask >> withMask [
	^self
]
