tests - set arithmetic
testIntersectionItself
	"self debug: #testIntersectionItself"
	
	| result |
	result :=  (self collectionWithoutEqualElements intersection: self collectionWithoutEqualElements).
	self assert: result size  = self collectionWithoutEqualElements size.
	self collectionWithoutEqualElements do: [ :each|
		self assert: (result includes: each) ].
	