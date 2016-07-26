rendering
actOnPresentationUpdated: ann
	container removeAllMorphs.
	container addMorphBack: (self morphFrom: ann presentation)