events-processing
onHidePreview
	|delta|
	self paneBrick isPreviewShown ifFalse: [ ^ self ].
	delta := ((self width - ((self width / 100) * (100 - self themer spotterThemer previewWidthPercent)))@0) negated / 2.
	self bounds: (self bounds expandBy: delta rounded).
	self paneBrick hidePreview.
	self doLayout