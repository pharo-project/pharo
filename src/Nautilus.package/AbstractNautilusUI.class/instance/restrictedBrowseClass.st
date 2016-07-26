menus behavior
restrictedBrowseClass
	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseClasses: {class} ]