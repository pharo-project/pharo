drawing
nonCachingFullDrawOn: aCanvas
	
	"A HandMorph has unusual drawing requirements:
		1. the hand itself (i.e., the cursor) appears in front of its submorphs
		2. morphs being held by the hand cast a shadow on the world/morphs below
	The illusion is that the hand plucks up morphs and carries them above the world."
	"Note: This version does not cache an image of the morphs being held by the hand.
	 Thus, it is slower for complex morphs, but consumes less space."

	submorphs isEmpty ifTrue: [^ self drawOn: aCanvas].  "just draw the hand itself"
	aCanvas asShadowDrawingCanvas
		translateBy: self shadowOffset during:[:shadowCanvas| | shadowForm |
		"Note: We use a shadow form here to prevent drawing
		overlapping morphs multiple times using the transparent
		shadow color."
		shadowForm := self shadowForm.
"
shadowForm displayAt: shadowForm offset negated. Display forceToScreen: (0@0 extent: shadowForm extent).
"
		shadowCanvas paintImage: shadowForm at: shadowForm offset.  "draw shadows"
	].
	"draw morphs in front of shadows"
	self drawSubmorphsOn: aCanvas.
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
