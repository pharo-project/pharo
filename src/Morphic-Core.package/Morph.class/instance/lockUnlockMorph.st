menus
lockUnlockMorph
	"If the receiver is locked, unlock it; if unlocked, lock it"

	self isLocked ifTrue: [self unlock] ifFalse: [self lock]