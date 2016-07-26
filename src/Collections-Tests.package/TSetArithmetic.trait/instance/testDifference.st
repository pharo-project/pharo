tests - set arithmetic
testDifference
	"Answer the set theoretic difference of two collections."
	"self debug: #testDifference"
	
	| difference |
	self assert: (self collectionWithoutEqualElements difference: self collectionWithoutEqualElements) isEmpty.
	self assert: (self empty difference: self collectionWithoutEqualElements) isEmpty.
	difference := (self collectionWithoutEqualElements difference: self empty).
	self assert: difference size = self collectionWithoutEqualElements	 size.
	self collectionWithoutEqualElements do: [ :each |
		self assert: (difference includes: each)].
