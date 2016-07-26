menus
loadClassSelection
	classSelection ifNil: [ ^self ].
	(self packageClasses detect: [ :ea | ea className = classSelection ] ifNone: [ ^self ]) load.
	self methodsForSelectedClass do: [ :m | m load ].