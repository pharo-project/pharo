accessing
primeFactors
	"Return all primeFactors of myself"
	"( 106260 primeFactors fold: [ :a :b | a * b ]) = 106260"
	
	^ Array streamContents: [ :s | self primeFactorsOn: s ]