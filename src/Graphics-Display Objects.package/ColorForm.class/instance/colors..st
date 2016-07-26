accessing
colors: colorList
	"Set my color palette to the given collection."

	| colorArray colorCount newColors |
	colorList ifNil: [
		colors := cachedDepth := cachedColormap := nil.
		^ self].

	colorArray := colorList asArray.
	colorCount := colorArray size.
	newColors := Array new: (1 bitShift: self depth).
	1 to: newColors size do: [:i |
		i <= colorCount
			ifTrue: [newColors at: i put: (colorArray at: i)]
			ifFalse: [newColors at: i put: Color transparent]].

	colors := newColors.
	cachedDepth := nil.
	cachedColormap := nil.
