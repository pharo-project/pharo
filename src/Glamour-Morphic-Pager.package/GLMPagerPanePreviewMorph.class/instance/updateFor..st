actions
updateFor: aMorph

	thumbnail := aMorph taskThumbnailOfSize: self maxExtent.
	self image: thumbnail form.
	
	self position: position - ((thumbnail width / 2)@(thumbnail height + self defaultBottomMargin + self defaultBorderWidth)).