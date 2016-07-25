menus behavior
restrictedBrowseSuperclasses

	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseClasses: class withAllSuperclasses ]