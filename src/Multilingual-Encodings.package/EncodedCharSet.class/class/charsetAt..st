class methods
charsetAt: encoding

	^ EncodedCharSets at: encoding + 1 ifAbsent: [Unicode].
