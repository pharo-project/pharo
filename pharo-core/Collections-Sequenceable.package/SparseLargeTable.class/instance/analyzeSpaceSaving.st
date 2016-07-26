private
analyzeSpaceSaving

	| total elems tablesTotal nonNilTables |
	total := size - base + 1.
	elems := 0.
	base to: size do: [:i | (self at: i) ~= defaultValue ifTrue: [elems := elems + 1]].
	tablesTotal := self basicSize.
	nonNilTables := 0.
	1 to: self basicSize do: [:i | (self basicAt: i) ifNotNil: [nonNilTables := nonNilTables + 1]].

	^ String streamContents: [:strm |
		strm nextPutAll: 'total: '.
		strm nextPutAll: total printString.
		strm nextPutAll: ' elements: '.
		strm nextPutAll: elems printString.
		strm nextPutAll: ' tables: '.
		strm nextPutAll: tablesTotal printString.
		strm nextPutAll: ' non-nil: '.
		strm nextPutAll: nonNilTables printString.
	].

