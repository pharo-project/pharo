*Athens-Morphic
displaySelectionBlock: aSelBlock inLine: line onAthensCanvas: anAthensCanvas
	(selectionStart notNil and: [ selectionStop notNil and: [ selectionStart ~= selectionStop ] ])
		ifTrue: [ 
			| startIdx stopIdx selSartIdx selStopIdx selBlockRange selRange |
			startIdx := aSelBlock first stringIndex.
			stopIdx := aSelBlock last stringIndex.
			selSartIdx := selectionStart stringIndex.
			selStopIdx := selectionStop stringIndex.
			selBlockRange := startIdx to: stopIdx.
			selRange := selSartIdx to: selStopIdx.
			((selBlockRange rangeIncludes: selSartIdx + 1)
				or: [ 
					(selBlockRange rangeIncludes: selStopIdx - 1)
						or: [ (selRange rangeIncludes: startIdx + 1) or: [ selRange rangeIncludes: stopIdx - 1 ] ] ])
				ifTrue: [ ^ self ] ].	"regular selection and this selection block overlap"
	aSelBlock displayInLine: line onAthensCanvas: anAthensCanvas