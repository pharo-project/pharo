writing
writeInto: aWriter
	aWriter writeInt: version.
	aWriter writeByteString: packageName.

	aWriter writeInt: traits size.
	traits do: [ :each | each writeInto: aWriter ].

	aWriter writeInt: classes size.
	classes do: [ :each | each writeInto: aWriter ].

	aWriter writeInt: extensionMethods size.
	extensionMethods do: [ :e | e writeInto: aWriter ].

	aWriter flush.