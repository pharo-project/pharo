I represents the windows frame for window dragging or resizing when fast dragging or fast resizing is wanted (when  UITheme currentSettings fastDragging is set to true).
For window resizing, I'm created by a corner or an edge grip when the mouse is clicked on it (see SystemWindow>>doFastWindowReframe:). For window dragging, I'm created when the top window bar is clicked (see SystemWindow>>doFastFrameDrag:).  I'm always created with the same bounds as the target window bounds. The mouse focus is given to me and my bounds are changed while the hand is moving. On mouse up, the window bounds is set to my own bounds and then I'm deleted.

Instance Variables
	location:		<Symbol>
	startGap:		<Point>
	target:		<SystemWindow>

location
	- The symbol representing the corner or the edge (#topLeft, #top, #topRight .... or #left). I'm set to nil for window dragging

startGap
	- For window dragging, keep track of the distance between the window top bar first click location and the window position

target
	- The window to be resized or dragged
