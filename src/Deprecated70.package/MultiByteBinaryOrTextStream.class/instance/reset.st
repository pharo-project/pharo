positioning
reset

	super reset.
	isBinary ifNil: [isBinary := false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection := collection asString.
		isBinary := true].

	self converter. "ensure that we have a converter."