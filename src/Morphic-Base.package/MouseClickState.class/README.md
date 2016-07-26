MouseClickState is a simple class managing the distinction between clicks, double clicks, and drag operations. It has been factored out of HandMorph due to the many instVars.

Instance variables:
	clickClient 	<Morph>		The client wishing to receive #click:, #dblClick:, or #drag messages
	clickState 	<Symbol>	The internal state of handling the last event (#firstClickDown, #firstClickUp, #firstClickTimedOut)
	firstClickDown 	<MorphicEvent>	The #mouseDown event after which the client wished to receive #click: or similar messages
	firstClickUp 	<MorphicEvent>	The first mouse up event which came in before the double click time out was exceeded (it is sent if there is a timout after the first mouse up event occured)
	firstClickTime 	<Integer>	The millisecond clock value of the first event
	clickSelector 	<Symbol>	The selector to use for sending #click: messages
	dblClickSelector 	<Symbol>	The selector to use for sending #doubleClick: messages
	dblClickTime 	<Integer>	Timout in milliseconds for a double click operation
	dragSelector 	<Symbol>	The selector to use for sending #drag: messages
	dragThreshold 	<Integer>	Threshold used for determining if a #drag: message is sent (pixels!)
