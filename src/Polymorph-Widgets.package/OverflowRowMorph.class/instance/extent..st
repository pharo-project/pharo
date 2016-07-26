geometry
extent: aPoint
	"Refit the base morphs and potential 'more' button'"

	super extent: aPoint.
	self fitBaseMorphs.
	self defer: [self layoutChanged]
		"since possibly changing actual submorphs during layout processing"