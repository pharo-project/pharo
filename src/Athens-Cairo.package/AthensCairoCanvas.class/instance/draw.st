drawing
draw
	"Fill the shape (anObject) using currently selected paint
	an object should implement double-dispatch to currently selected paint"
	
	"set the trasformation matrix"
	self setPathMatrix.
	
	^ shape paintFillsUsing: paint on: self

