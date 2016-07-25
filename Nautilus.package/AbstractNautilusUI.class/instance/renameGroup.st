menus behavior
renameGroup

	| group |
	group := self selectedGroup.
	group ifNil: [ ^ self ].
	[self groupsManager renameAGroup: group ]
		on: GroupAlreadyExists 
		do:[ :ex | self alertGroupExisting: ex groupName ].
	self updatePackageGroupAndClassList