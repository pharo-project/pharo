menus behavior
mergeGroups

	| group |
	group := self mergeGroups: self packageWidget selectedGroups.
	group
		ifNotNil: [ 
			self groupsManager addAGroup: group.
			self selectedGroup: group.
			self updatePackageGroupAndClassList]
