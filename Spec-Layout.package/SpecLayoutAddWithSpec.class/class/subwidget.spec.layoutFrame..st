instance creation
subwidget: subwidget spec: aSpecSelector layoutFrame: aLayoutFrame

	^ self new
		subwidget: subwidget 
		spec: aSpecSelector 
		layoutFrame:  aLayoutFrame;
		yourself