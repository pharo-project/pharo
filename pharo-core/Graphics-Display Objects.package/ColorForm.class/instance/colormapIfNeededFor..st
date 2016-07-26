color mapping
colormapIfNeededFor: destForm
	| newMap color pv |
	(self hasNonStandardPalette or:[destForm hasNonStandardPalette]) ifFalse:[
		^self colormapIfNeededForDepth: destForm depth.
	].
	colors == nil ifTrue: [
		"use the standard colormap"
		^ super colormapIfNeededFor: destForm].

	(destForm depth = cachedDepth and:[cachedColormap isColormap]) 
		ifTrue: [^ cachedColormap].
	newMap := WordArray new: (1 bitShift: self depth).
	1 to: colors size do: [:i |
		color := colors at: i.
		pv := destForm pixelValueFor: color.
		(pv = 0 and:[color isTransparent not]) ifTrue:[pv := 1].
		newMap at: i put: pv].

	cachedDepth := destForm depth.
	^cachedColormap := ColorMap shifts: nil masks: nil colors: newMap.