submorphs-accessing
noteNewOwner: aMorph 
	"Go fullscreen if needed."

	super noteNewOwner: aMorph.
	(self isFullscreen and: [labelArea owner notNil]) ifTrue: [
		self
			beWithoutGrips;
			removeLabelArea;
			makeBorderless.
		(self submorphs size = 1 and: [self submorphs first isKindOf: PanelMorph])
			ifTrue: [self submorphs first roundedCorners: #()]]