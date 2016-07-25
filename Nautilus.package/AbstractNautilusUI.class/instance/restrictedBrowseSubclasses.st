menus behavior
restrictedBrowseSubclasses

	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseClasses: class withAllSubclasses]