actions
deleteIfPopUp
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [self topRendererOrSelf delete].
	(popUpOwner notNil and: [popUpOwner isMenuItemMorph]) ifTrue: [
		popUpOwner isSelected: false.
		(popUpOwner owner isMenuMorph)
			ifTrue: [popUpOwner owner deleteIfPopUp]].
