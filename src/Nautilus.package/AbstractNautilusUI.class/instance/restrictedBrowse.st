menus behavior
restrictedBrowse
	self selectedClass
		ifNil: [ self restrictedBrowsePackage ]
		ifNotNil: [ self restrictedBrowseClass ]