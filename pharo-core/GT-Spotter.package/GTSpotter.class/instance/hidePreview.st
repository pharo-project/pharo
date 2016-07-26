actions api
hidePreview

	self isPreviewVisible ifFalse: [ ^ self ].
	isPreviewVisible := false.
	self class dontShowPreview.
	self notifyHidePreview
	