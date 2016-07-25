accessing
allWordingsNotInSubMenus: verbotenSubmenuContentsList
	"Answer a collection of the wordings of all items and subitems, but omit the stay-up item, and also any items in any submenu whose tag is in verbotenSubmenuContentsList"

	self isStayUpItem ifTrue:[^ #()].
	subMenu ifNotNil:
		[^ (verbotenSubmenuContentsList includes: self contents asString)
			ifTrue:
				[#()]
			ifFalse:
				[subMenu allWordingsNotInSubMenus: verbotenSubmenuContentsList]].

	^ Array with: self contents asString