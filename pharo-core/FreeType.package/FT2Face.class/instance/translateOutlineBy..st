rendering
translateOutlineBy: aPoint
	| delta|
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primTranslateGlyphSlotOutline: delta.