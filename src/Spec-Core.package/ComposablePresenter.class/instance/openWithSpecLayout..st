api
openWithSpecLayout: aSpec
	"Build the widget using the spec name provided as argument and display it into a window"

	(window value notNil and: [ self needRebuild not ])
		ifTrue: [
			window value rebuildWithSpecLayout: aSpec ]
		ifFalse: [ 
			window value: (self defaultWindowModelClass new model: self).
			window value openWithSpecLayout: aSpec.
			self takeKeyboardFocus ].
		
	^ window value