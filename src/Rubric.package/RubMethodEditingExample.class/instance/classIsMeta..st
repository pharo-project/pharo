accessing
classIsMeta: aBoolean
	classIsMeta := aBoolean.
	self selector: nil.
	self changed: #selector.
	self changed: #classIsMeta.
	self updateCodeWith: ''
