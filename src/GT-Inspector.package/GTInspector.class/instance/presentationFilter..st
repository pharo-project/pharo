context
presentationFilter: aPresentationFilter
	"Set a filter for limiting the number of visible presentations.
	Presentation filter must not be nil. If you don't need any filter use GTInspectorNoneFilter instead"
	
	self
		assert: [ aPresentationFilter isNotNil ]
		description: [ 'Presentation filter must not be nil' ].
	
	presentationFilter := aPresentationFilter