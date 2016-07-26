accessing
className: aClassName
	className := aClassName copyWithout: Character cr.
	self selector: nil.
	self changed: #selector.
	self changed: #className.
	self selectedClass
		ifNotNil: [	self changed: #haveClassName]