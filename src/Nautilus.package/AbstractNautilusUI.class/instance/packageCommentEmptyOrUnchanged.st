comment pane
packageCommentEmptyOrUnchanged
	^ commentTextModel getText isEmpty
		or: [ commentTextModel getText asString = PackageManifest description ]