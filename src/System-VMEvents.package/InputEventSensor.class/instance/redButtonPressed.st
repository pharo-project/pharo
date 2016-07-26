mouse
redButtonPressed
	"Answer true if only the red mouse button is being pressed.
	This is the first mouse button, usually the left one."

	^(self mouseButtons bitAnd: 7) = 4
