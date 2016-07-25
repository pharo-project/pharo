creating
createNumber: aString
	"Create a number literal. Subclasses might want to refine this implementation."

	^ aString asNumber