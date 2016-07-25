grabbing/dropping
grabMorph: aMorph from: formerOwner 
	"Grab the given morph (i.e., add it to this hand and remove it from its
	current owner) without changing its position. This is used to pick up a
	morph under the hand's current position, versus attachMorph: which
	is used to pick up a morph that may not be near this hand."
	| grabbed offset targetPoint grabTransform fullTransform |
	self releaseMouseFocus.
	"Break focus"
	grabbed := aMorph.
	aMorph keepsTransform
		ifTrue: [grabTransform := fullTransform := IdentityTransform new]
		ifFalse: ["Compute the transform to apply to the grabbed morph"
			grabTransform := formerOwner
						ifNil: [IdentityTransform new]
						ifNotNil: [formerOwner grabTransform].
			fullTransform := formerOwner
						ifNil: [IdentityTransform new]
						ifNotNil: [formerOwner transformFrom: owner]].
	"targetPoint is point in aMorphs reference frame"
	targetPoint := fullTransform globalPointToLocal: self position.
	"but current position will be determined by grabTransform, so
	compute offset"
	offset := targetPoint
				- (grabTransform globalPointToLocal: self position).
	"apply the transform that should be used after grabbing"
	grabbed := grabbed transformedBy: grabTransform.
	grabbed == aMorph
		ifFalse: [grabbed setProperty: #addedFlexAtGrab toValue: true].
	"offset target to compensate for differences in transforms"
	grabbed position: grabbed position - offset asIntegerPoint.
	"And compute distance from hand's position" 
	targetOffset := grabbed position - self position.
	self addMorphBack: grabbed.
	grabbed justGrabbedFrom: formerOwner