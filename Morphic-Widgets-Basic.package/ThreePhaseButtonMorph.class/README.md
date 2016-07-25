A button morph with separate images for on, off, and pressed with the mouse. 

When the event actWhen occurs, send actionSelector with 'arguments' to target.  For other events, default to my eventHandler.  The current event is not supplied in the arguments to the actionSelector.  

image (a.k.a. onImage) may not be nil.  offImage and pressedImage may be nil.  nil there means be transparent and show the underlying object.  

Tools for debugging:
Display the images momentarily under program control (for positioning) (self is an instance).
	self state: #on.  self state: #off.
	self state: #pressed.  self state: #off.
Display a rectangle where the button is.
	Display fillWithColor: bounds + (self world viewBox origin).
	self invalidRect: bounds.