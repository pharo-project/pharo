*GT-SpotterExtensions-Core
gtBreadcrumbTitle
	|breadcrumbTitle currentHelpTopic|
	
	currentHelpTopic := self.	
	breadcrumbTitle:= currentHelpTopic title.
	[ currentHelpTopic owner notNil and: [currentHelpTopic owner owner notNil ] ] whileTrue: [ 
		currentHelpTopic := currentHelpTopic owner.
		breadcrumbTitle := breadcrumbTitle , ' > ', currentHelpTopic title  ].
	^ breadcrumbTitle 