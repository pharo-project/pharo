instance creation
fromUser: gridPoint
	"Answer an instance of me with bitmap initialized from the area of the 
	display screen designated by the user. The grid for selecting an area is 
	aPoint. Ensures that the returned form has positive extent."
	| rect |
	rect := Rectangle fromUser: gridPoint.
	^ self fromDisplay: (rect origin extent: (rect extent max: gridPoint))