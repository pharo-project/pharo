updating
updateList
	| items |
	items := self selectedItems.
	self requestView: MorphTreeChangeRequest updateList.
	self selectItems: items