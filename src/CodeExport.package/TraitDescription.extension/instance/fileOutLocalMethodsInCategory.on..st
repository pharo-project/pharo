*CodeExport
fileOutLocalMethodsInCategory: aSymbol on: aFileStream
	"File out only the local (non-trait) methods for this category."

	| selectors localSelectors |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.
   localSelectors := selectors select:[:s | self isLocalSelector: s ].
	"Overridden to preserve author stamps in sources file regardless"
	localSelectors do: [:sel |
		self printMethodChunk: sel on: aFileStream ].
	^ self