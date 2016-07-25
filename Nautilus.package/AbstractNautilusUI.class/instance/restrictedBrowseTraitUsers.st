menus behavior
restrictedBrowseTraitUsers
	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class |
			self restrictedBrowseTraitUsers: class users ]