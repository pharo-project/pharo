Extension { #name : 'String' }

{ #category : '*OpalCompiler-Core' }
String >> parseLiterals [

	self 
		deprecated: 'Use OCParser parseLiterals: instead of String>>parseLiterals' 
		on: '29/12/2024' 
		in: 'pharo13' 
		transformWith: 
		'`@receiver parseLiterals' -> 'OCParser parseLiterals: `@receiver'.
	^ OCParser parseLiterals: self
]
