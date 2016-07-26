display
drawMeOn: aCanvas 
	"Draw a small view of a morph in another place.  Guard against infinite recursion if that morph has a thumbnail of itself inside.  Now also works if the thing to draw is a plain Form rather than a morph."

	| viewedMorphBox scale c shrunkForm aWorld aFormOrMorph  |
	super drawOn: aCanvas.
	((aFormOrMorph := self formOrMorphToView) isForm) 
		ifTrue: [^self drawForForm: aFormOrMorph on: aCanvas].
	(((aFormOrMorph notNil and: [(aWorld := aFormOrMorph world) notNil]) 
		and: [aWorld ~~ aFormOrMorph or: [lastFormShown isNil]]) 
			and: [RecursionDepth + 1 < RecursionMax]) 
			ifTrue: 
				[RecursionDepth := RecursionDepth + 1.
				viewedMorphBox := aFormOrMorph fullBounds.
			scale :=  self scaleFor: viewedMorphBox in: self innerBounds.
				c := Display defaultCanvasClass extent: viewedMorphBox extent
							depth: aCanvas depth.
				c translateBy: viewedMorphBox topLeft negated
					during: 
						[:tempCanvas | 
						"recursion happens here"
						tempCanvas fullDrawMorph: aFormOrMorph].
				shrunkForm := c form 
							magnify: c form boundingBox
							by: scale
							smoothing: 1.
				lastFormShown := shrunkForm.
				RecursionDepth := RecursionDepth - 1]
			ifFalse: 
				["This branch used if we've recurred, or if the thumbnail views a World that's already been rendered once, or if the referent is not in a world at the moment"
				lastFormShown ifNotNil: [shrunkForm := lastFormShown]].
	shrunkForm ifNotNil: 
			[aCanvas paintImage: shrunkForm
				at: self center - shrunkForm boundingBox center]