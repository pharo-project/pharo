accessing
endEntry
	| c d cb |
	c := self contents.
	Display extent ~= DisplayScreen actualScreenSize
		ifTrue: ["Handle case of user resizing physical window"
			DisplayScreen startUp.
			frame := frame intersect: Display boundingBox.
			^ self clear; show: c].
	self compose.
	d := para extent y - frame bottom.
	d > 0
		ifTrue: ["Scroll up to keep all contents visible"
			cb := para characterBlockAtPoint: para compositionRectangle topLeft + (0 @ (d + self textStyle lineGrid)).
			self on: (c copyFrom: cb stringIndex to: c size).
			readLimit := position := collection size.
			^ self endEntry].
	self display