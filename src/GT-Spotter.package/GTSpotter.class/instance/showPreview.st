actions api
showPreview

	self isPreviewVisible ifTrue: [ ^ self ].
	isPreviewVisible := true.
	self class showPreview.
	self notifyShowPreview
	