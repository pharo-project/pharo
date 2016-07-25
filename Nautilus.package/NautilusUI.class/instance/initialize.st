initialization
initialize
	super initialize.
	categoryWidget := CategoryWidget model: self.
	methodWidget := MethodWidget model: self.
	classWidget := ClassWidget model: self.
	packageWidget := PackageWidget model: self.