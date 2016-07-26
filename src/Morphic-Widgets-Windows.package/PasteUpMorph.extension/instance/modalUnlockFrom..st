*Morphic-Widgets-Windows
modalUnlockFrom: aSystemWindow
	"Don't unlock the world! Unlock the submorphs
	that were not originally locked."
	
	|lockStates|
	lockStates := self
		valueOfProperty: #submorphLockStates
		ifAbsent: [^self].
	self removeProperty: #submorphLockStates.
	lockStates keysAndValuesDo: [:m :locked |
		locked ifFalse: [m unlock]]