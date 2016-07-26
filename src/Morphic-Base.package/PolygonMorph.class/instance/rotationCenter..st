geometry
rotationCenter: aPointOrNil
	"Set the new rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	| box |
	aPointOrNil isNil
		ifTrue: [self removeProperty: #referencePosition]
		ifFalse: [box := self bounds.
				self setProperty: #referencePosition toValue: box origin + (aPointOrNil * box extent)]
