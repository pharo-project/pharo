recategorized
handlerForMouseDown: anEvent
	"If we have a modal dialog then answer nil otherwise as usual.."
	
	^(self hasProperty: #submorphLockStates)
		ifFalse: [super handlerForMouseDown: anEvent]