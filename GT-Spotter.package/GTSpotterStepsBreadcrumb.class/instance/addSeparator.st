adding/removing
addSeparator
	|separator|
	separator := self newSeparator.
	self themer spotterThemer breadcrumbSeparatorStyleFor: separator.
	self themer spotterThemer breadcrumbSeparatorWidgetStyleFor: separator.
	self band addBrickBack: separator