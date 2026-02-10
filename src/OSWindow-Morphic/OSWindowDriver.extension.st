Extension { #name : 'OSWindowDriver' }

{ #category : '*OSWindow-Morphic' }
OSWindowDriver >> afterMainPharoWindowCreated: aWindow [
]

{ #category : '*OSWindow-Morphic' }
OSWindowDriver >> beforeMainPharoWindowClosed: aWindow [
]

{ #category : '*OSWindow-Morphic' }
OSWindowDriver class >> shutDown: quitting [

	quitting ifFalse: [ ^ self ].

	Current ifNil: [ ^ self ].

	"Clean driver"
	Current shutDown: quitting.
	Current := nil.

	"clean OSWindow worlds"
	WorldMorph extraWorldList copy
		select: [ :each | each isKindOf: OSWindowWorldMorph ]
		thenDo: [ :each | WorldMorph removeExtraWorld: each ].
	"Clean also active world"
	(ActiveWorld isKindOf: OSWindowWorldMorph)
		ifTrue: [ ActiveWorld := nil ]
]
