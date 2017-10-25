instance creation
subwidget: subwidget layoutFrame: aLayoutFrame

	^ self new
		subwidget: subwidget 
		layoutFrame: aLayoutFrame asSpecLayoutFrame; 
		yourself