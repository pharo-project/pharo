accessing
categoryList
	^ categoryList ifNil: [categoryList := OrderedCollection new]