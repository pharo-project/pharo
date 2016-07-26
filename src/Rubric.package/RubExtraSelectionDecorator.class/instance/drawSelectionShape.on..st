drawing
drawSelectionShape: aSelBlock on: aCanvas
	"Display a SelectionShape if it does not overlap vith the regular selection"
	
	| startIdx stopIdx  |
	
	startIdx := aSelBlock firstIndex.
	stopIdx := aSelBlock lastIndex.
	(self hasValidSelection) ifTrue: [ 
			| selSartIdx selStopIdx selBlockRange selRange |
			selSartIdx := self paragraph selectionStart stringIndex.
			selStopIdx := self paragraph selectionStop stringIndex.
			selBlockRange := startIdx to: stopIdx.
			selRange := selSartIdx to: selStopIdx.
			((selBlockRange rangeIncludes: selSartIdx + 1) or: [ 
					(selBlockRange rangeIncludes: selStopIdx - 1) 	or: [ 
							(selRange rangeIncludes: startIdx + 1) or: [ 
								selRange rangeIncludes: stopIdx - 1 ] 
							] 
					]
			)ifTrue: [ ^ self ] 
		].	"regular selection and this selection block overlap"
	aSelBlock drawOn: aCanvas 