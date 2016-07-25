mouse
waitButtonOrKeyboard
	"Wait for the user to press either any mouse button or any key. 
	Answer the current cursor location or nil if a keypress occured."

	| delay |
	delay := Delay forMilliseconds: 50.
	[self anyButtonPressed]
		whileFalse: [delay wait.
			self keyboardPressed
				ifTrue: [^ nil]].
	^ self cursorPoint
