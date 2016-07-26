metrics
numberOfLinesOfCode

	^ self annotationNamed: #numberOfLinesOfCode ifAbsentPut: [ self sourceCode lineCount ]