private
addAGroup

	| group |
	group := self groupManager groups createAnEmptyStaticGroup.
	groupTree updateList.
	groupTreeModel hardlySelectItem: group.
	GroupAnnouncer uniqueInstance announce: (AGroupHasBeenAdded group: group into: self)