button behavior
emptySelectedGroup
	| groupToRemove confirmation |
	self removeState
		ifFalse: [ ^ self ].
	groupToRemove := self groupTreeModel selection selectedNodePath last.
	groupToRemove isGroup
		ifTrue: [ 
			self selectedGroup
				ifNotNil: [ :group | 
					confirmation := UIManager default confirm: 'Do you really want to empty the group named ' , group name , ' ?'.
					confirmation
						ifFalse: [ ^ self ].
					group removeAllPackages.
					groupTree updateList.
					groupTreeModel hardlySelectItem: group ] ]