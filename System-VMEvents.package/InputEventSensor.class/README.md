An InputEventSensor is a replacement for the old Morphic EventSensor framework.
It updates its state when events are received so that all state based users of Sensor (e.g., Sensor keyboard, Sensor leftShiftDown, Sensor mouseButtons) will work exactly as before. The usage of these funtions is discouraged. 

Instance variables:
	mouseButtons <Integer>	- mouse button state as replacement for primMouseButtons
	mousePosition <Point>	- mouse position as replacement for primMousePt
	eventQueue <SharedQueue>	- an optional event queue for event driven applications
	modifiers		<Integer>	- modifier states

Class variables:

	ButtonDecodeTable

	KeyDecodeTable
