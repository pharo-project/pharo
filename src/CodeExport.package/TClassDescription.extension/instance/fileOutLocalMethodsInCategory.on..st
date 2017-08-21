*CodeExport
fileOutLocalMethodsInCategory: aSymbol on: aFileStream
	"File out only the local (non-trait) methods for this category."

	| selectors selectorsLocal |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.
   selectorsLocal := selectors select:[:s | self isLocalSelector: s ].
	"Overridden to preserve author stamps in sources file regardless"
	selectorsLocal do: [:sel |
		self printMethodChunk: sel on: aFileStream ].
	^ self