Class {
	#name : #OCScannerTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #testing }
OCScannerTest >> testAmbiguousSelector [
	"Non regression test.
	See http://code.google.com/p/pharo/issues/detail?id=2271
	and http://bugs.squeak.org/view.php?id=7491"

	'1@-1' parseLiterals.
	self assert: ('1@-1' parseLiterals at: 2) asString equals: '@-'
]

{ #category : #testing }
OCScannerTest >> testLiteralSymbols [
		
	((((32 to: 126) collect: [:ascii | Character value: ascii])) copyWithout: $`) , "backtick is not suppored by OBParser"
	#(':x:yourself' '::' 'x:yourself' '123' 'x0:1:2:' 'x.y.z' '1abc' 'a1b0c2' ' x' 'x ' '+x-y' '||' '--' '++' '+-' '+/-' '-/+' '<|>' '#x' '()' '[]' '{}' '')
		do: [:str |
			self assert: (self class compiler evaluate: str asSymbol printString) = str asSymbol
				description: 'in all case, a Symbol must be printed in an interpretable fashion']
]
