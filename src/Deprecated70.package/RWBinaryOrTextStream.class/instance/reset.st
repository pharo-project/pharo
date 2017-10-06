positioning
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary := false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection := collection asString.
		isBinary := true].
