A GLMBrick is the superclass of all Bricks. It subclasses cleaned morph with deleted layouting mechanism

Instance Variables
	brickBounds:		<GLMBrickBounds>
	ports:		<Dictionary>

brickBounds
	- wrapper object for brick geometry.  
		holds margin, padding properties.,
		responsible for calculation of outer/inner bounds depending on margin/padding

ports
	- dictionary of phlow bindings. is not implemented yet
