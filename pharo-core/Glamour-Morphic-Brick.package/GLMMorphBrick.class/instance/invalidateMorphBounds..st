brick-layout
invalidateMorphBounds: isForce
	| origin corner newBounds |
	isLayouting & (isForce not) ifTrue: [ ^ self ].
	origin := self paddingLeft @ self paddingTop.
	corner := self paddingRight @ self paddingBottom.
	newBounds := self globalBounds.
	newBounds := ((newBounds origin + origin) corner: (newBounds corner - corner)).
	
	self morph bounds: newBounds