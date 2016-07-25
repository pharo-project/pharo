submorphs-accessing
lineNumbersDisplayLabel
	^ self paragraphProvider lineNumbersRuler  
		ifNil: [ '+L' ]
		ifNotNil: [ 'L' ]