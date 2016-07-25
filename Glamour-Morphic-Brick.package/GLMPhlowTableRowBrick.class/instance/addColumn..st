adding
addColumn: aColumn
	|brick|
	brick := (aColumn computation phlowValue: self entity) asBrick.
	brick := aColumn style phlowValue: brick.
	^ self addBrickBack: brick