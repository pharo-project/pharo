composition
compositionRectangle
	| e tl |
	e := self compositionExtent.
	tl := (self container insetBy: self margins) topLeft.
	^ tl corner: (tl x + e x - self margins right - (self margins left max: self cursorWidth)) @ e y