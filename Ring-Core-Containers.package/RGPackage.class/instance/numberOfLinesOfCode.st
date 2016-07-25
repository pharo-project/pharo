metrics
numberOfLinesOfCode
	
	^ self 
		annotationNamed: #numberOfLinesOfCode
		ifAbsentPut: [
			| number |
			number := self classes inject: 0 into: [ :sum :each | sum + each numberOfLinesOfCode + each theMetaClass numberOfLinesOfCode ].
			number := self methods inject: number into: [ :sum :each | sum + each numberOfLinesOfCode ].
			number ]