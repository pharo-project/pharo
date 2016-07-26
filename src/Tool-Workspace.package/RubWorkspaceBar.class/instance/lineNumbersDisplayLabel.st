managing line numbers
lineNumbersDisplayLabel
	^ self paragraphProvider lineNumbersRuler  
		ifNil: [ '+L' ]
		ifNotNil: [ 'L' ]