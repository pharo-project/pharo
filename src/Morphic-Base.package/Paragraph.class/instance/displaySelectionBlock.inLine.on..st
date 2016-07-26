display
displaySelectionBlock: aSelBlock inLine: line on: aCanvas 
	"Display a SelectionBlock if it does not overlap vith the regular selection"
	
	(selectionStart notNil and: [selectionStop notNil and: [selectionStart ~= selectionStop]])
		ifTrue: [	| startIdx stopIdx selSartIdx selStopIdx selBlockRange selRange |
			startIdx := aSelBlock first stringIndex.
			stopIdx := aSelBlock last stringIndex.
			selSartIdx := selectionStart stringIndex.
			selStopIdx := selectionStop stringIndex.
			selBlockRange := (startIdx) to: (stopIdx).
			selRange := (selSartIdx) to: (selStopIdx).
			((selBlockRange rangeIncludes: selSartIdx + 1) or: [(selBlockRange rangeIncludes: selStopIdx - 1) 
				or: [(selRange rangeIncludes: startIdx + 1) or: [selRange rangeIncludes: stopIdx - 1]]])
				ifTrue: [^ self]]. "regular selection and this selection block overlap"
	aSelBlock displayInLine: line on: aCanvas
