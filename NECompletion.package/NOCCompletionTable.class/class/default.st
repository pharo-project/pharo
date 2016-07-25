instance creation
default
	^ table ifNil: [table := self new entriesPerPrefix: 40]