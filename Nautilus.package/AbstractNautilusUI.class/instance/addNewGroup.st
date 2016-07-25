group
addNewGroup
	| group |
	[ group := self groupsManager createAnEmptyStaticGroup ]
		on: GroupAlreadyExists
		do: [ :ex | self alertGroupExisting: ex groupName ].
	GroupAnnouncer uniqueInstance announce: (AGroupHasBeenAdded group: group into: self)