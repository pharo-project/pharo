scrolling
scrollBy: aPoint
	"Scroll receiver by some offset"
	| x y  |
	
	x := offset x + aPoint x.
	y := offset y + aPoint y.


	[y < 0] whileTrue: [ | h |
		h := layout height.
		y := layout extendOneLineUp 
			ifFalse: [ 0 ]
			ifTrue: [ y + (layout height - h) ].
	 ].

	aPoint y > 0 ifTrue: [
		self extendLayoutDown: y ].

	"Don't scroll beyond layout"
	x := (x min: layout width - extent x) max: 0. 
	y := (y min: layout height - extent y) max: 0.

	offset := x@y.	
	self compact.
	self announceViewChanged.