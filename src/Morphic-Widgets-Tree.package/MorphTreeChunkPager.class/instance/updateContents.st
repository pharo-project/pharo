initailize-release
updateContents
	treeMorph vIsScrollable 
		ifFalse: [self atBottom: true].
	super updateContents