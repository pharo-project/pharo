actions
browseClassRefs
	self currentMethod ifNotNil: [ :method | 
		model browseClassRefsOf: method methodClass ]