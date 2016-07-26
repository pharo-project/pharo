menus behavior
inspectMethods
	 self selectedClass ifNil: [ ^ self ].
	 self selectedClass methods inspect