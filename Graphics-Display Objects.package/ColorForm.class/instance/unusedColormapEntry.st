private
unusedColormapEntry
	"Return the index of an unused color map entry, or zero if there isn't one."

	| tallies |
	tallies := self tallyPixelValues.
	1 to: tallies size do: [:i |
		(tallies at: i) = 0 ifTrue: [^ i]].
	^ 0
