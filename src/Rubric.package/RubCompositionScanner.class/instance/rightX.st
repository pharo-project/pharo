accessing
rightX
	"Meaningful only when a line has just been composed -- refers to the 
	line most recently composed. This is a subtrefuge to allow for easy 
	resizing of a composition rectangle to the width of the maximum line. 
	Useful only when there is only one line in the form or when each line 
	is terminated by a carriage return. Handy for sizing menus and lists."

	breakAtSpace ifTrue: [^ spaceX].

	^ destX.
