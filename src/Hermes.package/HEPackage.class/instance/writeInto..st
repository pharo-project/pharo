writing
writeInto: aWriter
	self sortContent.

	aWriter writeInt32: version.
	aWriter writeByteString: packageName.

	aWriter writeInt32: traits size.
	traits do: [ :each | each writeInto: aWriter ].

	aWriter writeInt32: classes size.
	classes do: [ :each | each writeInto: aWriter ].

	aWriter writeInt32: extensionMethods size.
	extensionMethods do: [ :e | e writeInto: aWriter ].

	aWriter flush.