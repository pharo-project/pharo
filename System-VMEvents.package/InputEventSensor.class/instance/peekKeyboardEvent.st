private events
peekKeyboardEvent
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running kbdTest or the InterpreterSimulator in Morphic"

	^eventQueue findFirst: [:buf | self isKbdEvent: buf]