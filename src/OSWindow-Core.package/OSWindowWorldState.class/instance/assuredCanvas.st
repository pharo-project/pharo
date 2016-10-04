as yet unclassified
assuredCanvas

	(canvas isNil or: [(canvas extent ~= viewBox extent) or: [canvas form depth ~= Display depth]])
		ifTrue:
			["allocate a new offscreen canvas the size of the window"
			self canvas: (display defaultCanvasClass extent: viewBox extent)].
	^ self canvas