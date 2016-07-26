geometry etoy
rotationCenter: aPointOrNil
	"Set the new rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	| newRef box |
	aPointOrNil isNil
		ifTrue: [self removeProperty: #referencePosition.
			self removeProperty: #originalCenter.
			self removeProperty: #originalAngle. ]
		ifFalse: [ box := self bounds.
				newRef := box origin + (aPointOrNil * box extent).
				self setRotationCenterFrom: newRef ].

