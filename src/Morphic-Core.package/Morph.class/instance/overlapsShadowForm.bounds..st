geometry
overlapsShadowForm: itsShadow bounds: itsBounds
	"Answer true if itsShadow and my shadow overlap at all"
	| andForm overlapExtent |
	overlapExtent := (itsBounds intersect: self fullBounds ifNone: [ ^ false ]) extent.
	overlapExtent > (0 @ 0)
		ifFalse: [^ false].
	andForm := self shadowForm.
	overlapExtent ~= self fullBounds extent
		ifTrue: [andForm := andForm
						contentsOfArea: (0 @ 0 extent: overlapExtent)].
	andForm := andForm
				copyBits: (self fullBounds translateBy: itsShadow offset negated)
				from: itsShadow
				at: 0 @ 0
				clippingBox: (0 @ 0 extent: overlapExtent)
				rule: Form and
				fillColor: nil.
	^ andForm bits
		anySatisfy: [:w | w ~= 0]