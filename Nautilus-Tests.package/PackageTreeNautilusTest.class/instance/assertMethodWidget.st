asserting
assertMethodWidget
	"Assert changes for new fast table"
	AbstractWidget listClass = FTPluggableIconListMorphAdaptor 
		ifTrue: [ ^ self assertMethodWidgetAdaptor ].
	^ self assertMethodWidgetPluggableListMorph