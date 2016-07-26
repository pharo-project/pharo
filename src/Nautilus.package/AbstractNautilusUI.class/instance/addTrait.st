menus behavior
addTrait

	self selectedPackage ifNotNil: [:package | self addTraitIn: package ]
	
	" No need of update, announcements will do the job "