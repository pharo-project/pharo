*Polymorph-Widgets
roundedCorners: anArray
	"Set the corners to round."

	anArray = #(1 2 3 4)
		ifTrue: [self removeProperty: #roundedCorners]
		ifFalse: [self setProperty: #roundedCorners toValue: anArray].
	self changed