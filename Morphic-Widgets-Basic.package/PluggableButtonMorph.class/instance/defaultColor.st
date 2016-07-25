initialization
defaultColor
	"answer the default color/fill style for the receiver"
	^ self containingWindow ifNil: [Color lightGreen] ifNotNil: [:w | w defaultBackgroundColor]