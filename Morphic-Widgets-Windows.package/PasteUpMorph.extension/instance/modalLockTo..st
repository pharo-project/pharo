*Morphic-Widgets-Windows
modalLockTo: aSystemWindow
	"Don't lock the world nor the aSystemWindow! Lock the submorphs."
	
	|lockStates|
	
	lockStates := IdentityDictionary new.
	"lock all submorphs"
	self submorphsDo: [:m |
		lockStates at: m put: m isLocked.
		m lock].
	"don't lock the given window"
	aSystemWindow unlock.
	lockStates at: aSystemWindow put: aSystemWindow isLocked.
	
	self
		setProperty: #submorphLockStates
		toValue: lockStates