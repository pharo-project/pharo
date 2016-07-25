typing support
dispatch: aKeyboardEvent
	"Carry out the action associated with this character, if any.
	Type-ahead is passed so some routines can flush or use it."

	| return |
	return := [ :val | ^ val ].
	self dispatchEnterOn: aKeyboardEvent return: return.
	self dispatchCommandOn: aKeyboardEvent return: return.
	^ self normalCharacter: aKeyboardEvent