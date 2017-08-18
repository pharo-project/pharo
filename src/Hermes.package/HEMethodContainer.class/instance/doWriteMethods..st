writing
doWriteMethods: aWriter
	aWriter writeByteString: category.

	aWriter writeInt: methods size.
	methods do: [ :e | e writeInto: aWriter ].

	aWriter writeInt: classSideMethods size.
	classSideMethods do: [ :e | e writeInto: aWriter ].

	traitComposition writeInto: aWriter.
	classTraitComposition writeInto: aWriter