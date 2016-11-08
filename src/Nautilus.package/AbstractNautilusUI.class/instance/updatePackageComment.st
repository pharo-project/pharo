comment pane
updatePackageComment
	self selectedPackage packageComment = commentTextModel getText asString
		ifTrue: [ ^ self ].
	self selectedPackage
		packageComment: commentTextModel getText asString printString