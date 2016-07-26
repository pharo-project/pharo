default accessing
oppositeDelimiterSelection
	^ OppositeDelimiterSelection
		ifNil: [ 
			OppositeDelimiterSelection := RubTextSelectionColor new
				colorBlock: [ Color black ];
				backgroundColorBlock: [ Color green muchLighter ];
				emphasisCode: 0;
				yourself ]