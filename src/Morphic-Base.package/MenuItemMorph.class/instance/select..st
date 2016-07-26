selecting
select: evt
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu
			popUpAdjacentTo: self adjacentTo
			forHand: evt hand
			from: self.
		subMenu selectItem: nil event: evt].