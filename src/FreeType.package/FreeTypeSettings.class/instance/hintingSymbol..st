accessing
hintingSymbol: aSymbol
	"#Full, #Light, #Normal or #None"
	monoHinting := aSymbol = #Full.
	lightHinting := aSymbol = #Light.
	hinting := monoHinting or:[lightHinting or:[aSymbol = #Normal]].
	FreeTypeCache current removeAll.
	FreeTypeFont allSubInstances do:[:each | each clearCachedMetrics].
	Paragraph allSubInstances do:[:each | each composeAll].
	World restoreMorphicDisplay.

