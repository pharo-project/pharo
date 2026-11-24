Extension { #name : 'HandMorph' }

{ #category : '*Morphic-Halos' }
HandMorph >> halo [
	"Return the halo associated with this hand, if any. A halo that left the world is gone"

	^ (self valueOfProperty: #halo) ifNotNil: [ :halo | halo owner ifNotNil: [ halo ] ]
]

{ #category : '*Morphic-Halos' }
HandMorph >> halo: newHalo [
	"Set halo associated with this hand"
	| oldHalo |
	oldHalo := self halo.
	(oldHalo isNil or:[oldHalo == newHalo]) ifFalse:[oldHalo delete].
	newHalo
		ifNil:[self removeProperty: #halo]
		ifNotNil:[self setProperty: #halo toValue: newHalo]
]

{ #category : '*Morphic-Halos' }
HandMorph >> obtainHalo: aHalo [
	"Used for transfering halos between hands"

	self halo == aHalo
		ifTrue: [ ^ self ].
	"Find former owner"
	self world hands detect: [ :hand | hand halo == aHalo ] ifFound: [ :formerOwner | formerOwner releaseHalo: aHalo ].
	self halo: aHalo
]

{ #category : '*Morphic-Halos' }
HandMorph >> releaseHalo: aHalo [
	"Used for transfering halos between hands"
	self removeProperty: #halo
]

{ #category : '*Morphic-Halos' }
HandMorph >> removeHalo [
	"remove the receiver's halo (if any)"
	self halo ifNotNil: [ :h | self removeHaloAround: h target ]
]

{ #category : '*Morphic-Halos' }
HandMorph >> removeHaloAround: aMorph [
	"remove the receiver's halo associated to aMorph (if any)"
	| halo |
	halo := self halo.
	halo
		ifNil: [ ^ self ].
	halo target == aMorph
		ifFalse: [ ^ self ].
	self removeProperty: #halo.
	halo delete
]

{ #category : '*Morphic-Halos' }
HandMorph >> removeHaloFromClick: anEvent on: aMorph [
	| halo |
	halo := self halo
				ifNil: [^ self].
	(halo target hasOwner: self)
		ifTrue: [^ self].
	(halo staysUpWhenMouseIsDownIn: aMorph)
		ifFalse: [self removeHalo]
]

{ #category : '*Morphic-Halos' }
HandMorph >> selectedObject [
	"answer the selected object for the hand or nil is none"
	| halo |
	halo := self halo.
	halo ifNil: [^ nil].
	^ halo target renderedMorph
]
