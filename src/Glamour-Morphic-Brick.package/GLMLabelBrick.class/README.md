A GLMLabelBrick is standard label Brick, that can shrink text if it's size becomes smaller then actual text size.

Text shrinking works out of box, you don't need to do anything

Instance Variables
	dotsBrick		<GLMStringBrick>
	isDotsAdded:		<boolean>
	stringBrick:		<GLMStringBrick>

dotsBrick
	- represents a Brick that is added at the end of the text when Brick's size becomes smaller than actual text size

isDotsAdded
	- true if dotsBrick is currently added visible, false otherwise

stringBrick
	- represents actual label text Brick
