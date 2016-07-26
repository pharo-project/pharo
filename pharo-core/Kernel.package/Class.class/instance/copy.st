copying
copy 
	"Answer a copy of the receiver without a list of subclasses"
	| newClass |
	newClass := self class copy new
		superclass: superclass;
		methodDict: self methodDict copy;
		setFormat: format;
		setName: name;
		organization: self organization copy;
		classPool: classPool copy;
		sharedPools: sharedPools copy.
	self class instSize+1 to: self class instSize do:
		[:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
	newClass classLayout: (layout copy host: newClass).
	^ newClass