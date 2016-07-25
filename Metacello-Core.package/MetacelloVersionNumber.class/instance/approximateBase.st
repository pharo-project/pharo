accessing
approximateBase

	| base |
	base := self copyFrom: 1 to: self size - 1.
	base at: base size put: (base at: base size) + 1.
	^base