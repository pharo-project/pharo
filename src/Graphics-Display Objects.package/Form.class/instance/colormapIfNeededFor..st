color mapping
colormapIfNeededFor: destForm
	"Return a ColorMap mapping from the receiver to destForm."
	(self hasNonStandardPalette or:[destForm hasNonStandardPalette]) 
		ifTrue:[^self colormapFromARGB mappingTo: destForm colormapFromARGB]
		ifFalse:[^self colormapIfNeededForDepth: destForm depth]