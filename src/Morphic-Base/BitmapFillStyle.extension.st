Extension { #name : #BitmapFillStyle }

{ #category : #'*Morphic-Base-Balloon' }
BitmapFillStyle >> grabNewGraphicIn: aMorph event: evt [ 
	"Used by any morph that can be represented by a graphic"
	| fill |
	fill := Form fromUser.
	fill boundingBox area = 0
		ifTrue: [^ self].
	self form: fill.
	self direction: fill width @ 0.
	self normal: 0 @ fill height.
	aMorph changed
]

{ #category : #'*Morphic-Base-Balloon' }
BitmapFillStyle >> newForm: aForm forMorph: aMorph [
	self form: aForm.
	self direction: (aForm width @ 0).
	self normal: (0 @ aForm height).
	aMorph changed.
]
