pool initialization
initialize
	"EventSensorConstants initialize"
	RedButtonBit := 4.
	BlueButtonBit := 2.
	YellowButtonBit := 1.

	ShiftKeyBit := 1.
	CtrlKeyBit := 2.
	OptionKeyBit := 4.
	CommandKeyBit := 8.

	"Types of events"
	EventTypeNone := 0.
	EventTypeMouse := 1.
	EventTypeKeyboard := 2.
	EventTypeDragDropFiles := 3.
	EventTypeMenu := 4.
	EventTypeWindow := 5.

	"Press codes for keyboard events"
	EventKeyChar := 0.
	EventKeyDown := 1.
	EventKeyUp := 2.
	
	"Window event action codes"
	WindowEventMetricChange := 1. " size or position of window changed - value1-4 are left/top/right/bottom values "
	WindowEventClose := 2. " window close icon pressed "
	WindowEventIconise := 3. " window iconised  or hidden etc "
	WindowEventActivated :=4. " window made active - some platforms only - do not rely upon this "
	WindowEventPaint := 5. " window area (in value1-4) needs updating. Some platforms do not need to send this, do not rely on it in image "	
