examples
example4
	" 
	This example demostrates that same path could be reused multiple times when drawing.
	First, we creating a path, and later we can use it in #drawShape: command.
	self example4
	"

	| surf path |
	
	surf := self newSurface: 100@100.
	
	path :=  surf createRectanglePath: (0@ 0 corner: 20@ 20).
	
	surf drawDuring: [:can |
		surf clear.
		can pathTransform loadIdentity.
		can setPaint: (Color red).
		can pathTransform translateX: 5 Y: 5.
		can drawShape: path.
		can setPaint: (Color blue).
		can pathTransform translateX: 5 Y: 5.
		can drawShape: path.
	].
	Display getCanvas drawImage: 	surf asForm  at: 0@0
