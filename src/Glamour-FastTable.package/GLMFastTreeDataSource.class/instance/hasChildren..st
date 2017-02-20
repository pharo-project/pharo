testing
hasChildren: anItem
	^ self glamourPresentation hasChildrenBlock 
		ifNil: [ anItem children isNotEmpty ]
		ifNotNil: [ :aBlock | aBlock cull: anItem data ]