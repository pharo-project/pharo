*Spec-MorphicAdapters
openWorldWithSpecLayout: aSpec
	"Build the widget using the spec name provided as argument and display it into the world"

	(window value notNil and: [ self needRebuild not ])
		ifTrue: [
			window value rebuildWithSpecLayout: aSpec ]
		ifFalse: [ 
			window value: (WorldPresenter new model: self).
			window value openWithSpecLayout: aSpec.
			self takeKeyboardFocus ].
		
	^ window value