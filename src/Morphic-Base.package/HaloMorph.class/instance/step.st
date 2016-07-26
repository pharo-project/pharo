stepping
step
	| newBounds |
	target
		ifNil: [^ self].
	newBounds := target isWorldMorph
				ifTrue: [target bounds]
				ifFalse: [self localHaloBoundsFor: target renderedMorph].
	newBounds = self bounds
		ifTrue: [^ self].
	newBounds extent = self bounds extent
		ifTrue: [^ self position: newBounds origin].
	growingOrRotating
		ifFalse: [submorphs size > 1
				ifTrue: [self addHandles]].
	"adjust halo bounds if appropriate"
	self bounds: newBounds