actions
browseSelectedObjectClass
	self selectedObjectDo: [ :anObject | Smalltalk tools browser openOnClass: anObject class ]