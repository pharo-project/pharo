drawing
fillOval: r color: c borderWidth: borderWidth borderColor: borderColor
	"Draw a filled and outlined oval"
	"Note: The optimization test below should actually read:
		self ifNoTransformWithIn: (r insetBy: borderWidth // 2)
	but since borderWidth is assumed to be very small related to r we don't check it."

	(self ifNoTransformWithIn: r)
		ifTrue:[^super fillOval: r color: c borderWidth: borderWidth borderColor: borderColor].

	^self drawOval: (r insetBy: borderWidth // 2) 
			color: c 
			borderWidth: borderWidth 
			borderColor: borderColor