actions api
togglePreview

	self isPreviewVisible
		ifTrue: [ self hidePreview ]
		ifFalse: [ self showPreview ]