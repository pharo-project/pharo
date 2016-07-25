menus behavior
removeGroups

	self okToChange ifFalse: [ ^ self ].
	self packageWidget selectedGroups
		ifNotEmpty:[:groups |
			self packageWidget resetGroupsSelection.
			groups do: [:group | self groupsManager removeAGroup: group ].
			self selectedGroup: nil].
		