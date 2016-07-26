analyzing
colorsUsed
	"Return a list of the Colors this form uses."

	| tallies tallyDepth usedColors |
	tallies := self tallyPixelValues.
	tallyDepth := (tallies size log: 2) asInteger.
	usedColors := OrderedCollection new.
	tallies doWithIndex: [:count :i |
		count > 0 ifTrue: [
			usedColors add: (Color colorFromPixelValue: i - 1 depth: tallyDepth)]].
	^ usedColors asArray
