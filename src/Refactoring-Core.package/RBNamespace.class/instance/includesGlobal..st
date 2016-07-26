testing
includesGlobal: aSymbol 
	(self hasRemoved: aSymbol) ifTrue: [^false].
	(self includesClassNamed: aSymbol) ifTrue: [^true].
	environment at: aSymbol ifAbsent: [^false].
	^true