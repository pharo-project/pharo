events-processing
onShowPreview

	|delta|
	self paneBrick isPreviewShown ifTrue: [ ^ self ].
	delta := (((self width * 100) / (100 - self themer spotterThemer previewWidthPercent) - self width)@0) / 2.
	self bounds: (self bounds expandBy: delta rounded).
	self paneBrick showPreview.
	self doLayout