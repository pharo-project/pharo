dispatching
dispatch: aKeyboardEvent
	self do: [ :targetToDispatch |
		targetToDispatch dispatch: KMBuffer uniqueInstance buffer copy.
		aKeyboardEvent wasHandled ifTrue: [ ^self ].
	].
	"This should be a noMatch event"
	aKeyboardEvent wasHandled ifFalse: [ KMBuffer uniqueInstance clearBuffer ]