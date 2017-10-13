writing
doWriteMethods: aWriter
	aWriter writeByteString: category.

	aWriter writeInt32: methods size.
	methods do: [ :e | e writeInto: aWriter ].

	aWriter writeInt32: classSideMethods size.
	classSideMethods do: [ :e | e writeInto: aWriter ].

	traitComposition writeInto: aWriter.
	classTraitComposition writeInto: aWriter