accessing
classes
	^ classTable ifNil: [classTable := self new entriesPerPrefix: 40]