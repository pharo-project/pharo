geometry
localVisibleSubmorphBounds
	"Answer, in my coordinate system, the bounds of all my visible submorphs (or nil if no visible submorphs)"
	| subBounds |
	subBounds := nil.
	self submorphsDo: [:m |
		(m visible) ifTrue: [
			subBounds
				ifNil: [subBounds := m fullBounds copy]
				ifNotNil: [subBounds := subBounds quickMerge: m fullBounds]]
			].
	^subBounds