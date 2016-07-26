control
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."
	stayUp ifFalse: [self topRendererOrSelf delete].
	(popUpOwner notNil) ifTrue: [
		popUpOwner isSelected: false.
		popUpOwner deleteIfPopUp: evt].
	evt ifNotNil:[evt hand releaseMouseFocus: self].