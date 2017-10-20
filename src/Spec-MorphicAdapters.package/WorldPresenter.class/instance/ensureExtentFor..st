api
ensureExtentFor: aWidget

	self extent ifNotNil: [ :ext | DisplayScreen hostWindowSize: ext ].
	self initialExtent ifNotNil: [ :ext | DisplayScreen hostWindowSize: ext ].