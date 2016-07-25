drawing
drawSelectionShape: aSelBlock onAthensCanvas: anAthensCanvas
	"Display a SelectionShape if it does not overlap vith the regular selection"
	| startIdx stopIdx  |
	startIdx := aSelBlock firstIndex.
	stopIdx := aSelBlock lastIndex.
	self hasValidSelection ifTrue: [ 
		| selSartIdx selStopIdx selBlockRange selRange |
		
		selSartIdx := self selectionStart stringIndex.
		selStopIdx := self selectionStop stringIndex.
		selBlockRange := startIdx to: stopIdx.
		selRange := selSartIdx to: selStopIdx.
		((selBlockRange rangeIncludes: selSartIdx + 1)
				or: [ 
					(selBlockRange rangeIncludes: selStopIdx - 1)
						or: [ (selRange rangeIncludes: startIdx + 1) or: [ selRange rangeIncludes: stopIdx - 1 ] ] ])
				ifTrue: [ ^ self ] ].	"regular selection and this selection block overlap"
	aSelBlock drawOnAthensCanvas: anAthensCanvas