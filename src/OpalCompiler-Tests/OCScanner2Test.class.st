Class {
	#name : 'OCScanner2Test',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'testing' }
OCScanner2Test >> testLiteralSymbols [
	"backtick is not suppored by OCParser"
	(((32 to: 126) collect: [ :ascii | Character value: ascii ]) copyWithout: $`)
	,
		#( ':x:yourself' '::' 'x:yourself' '123' 'x0:1:2:' 'x.y.z' '1abc' 'a1b0c2' ' x' 'x ' '+x-y' '||' '--' '++' '+-' '+/-' '-/+'
		   '<|>' '#x' '()' '[]' '{}' '' ) do: [ :str |
			self
				assert: (OpalEvaluator new evaluate: str asSymbol printString) = str asSymbol
				description: 'in all case, a Symbol must be printed in an interpretable fashion' ] 
]
