brick-properties-events
onThemerChanged
	super onThemerChanged.
	
	self band subbricks withIndexDo: [ :each :index | index even ifTrue: [ self themer spotterThemer breadcrumbSeparatorWidgetStyleFor: each ] ]