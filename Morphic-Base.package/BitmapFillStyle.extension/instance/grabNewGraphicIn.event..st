*Morphic-Base-Balloon
grabNewGraphicIn: aMorph event: evt 
	"Used by any morph that can be represented by a graphic"
	| fill |
	fill := Form fromUser.
	fill boundingBox area = 0
		ifTrue: [^ self].
	self form: fill.
	self direction: fill width @ 0.
	self normal: 0 @ fill height.
	aMorph changed