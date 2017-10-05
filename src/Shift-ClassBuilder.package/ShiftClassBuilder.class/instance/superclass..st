accessing
superclass: aSuperclass
	aSuperclass ifNil:[superclassName := nil].
	superclass := aSuperclass