geometry etoy
rotationCenter: aPointOrNil
	"Set the new rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."

	| newRef box |
	aPointOrNil
		ifNotNil: [ box := self bounds.
			newRef := box origin + (aPointOrNil * box extent).
			self setRotationCenterFrom: newRef ]