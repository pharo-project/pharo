accessing
monitorType: aSymbol
	"#LCD or #CRT"
	subPixelAntiAliasing := aSymbol = #LCD.
	World restoreMorphicDisplay.