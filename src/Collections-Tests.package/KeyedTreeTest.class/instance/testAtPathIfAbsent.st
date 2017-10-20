tests
testAtPathIfAbsent
	"Test the at path if absent method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1) ifAbsent: []) = t2].
	self should: [(tree atPath: #(1 two) ifAbsent: []) = 'One-Two'].
	self should: [(tree atPath: #(1 three) ifAbsent: []) = 'One-Three'].
	self should: [(tree atPath: #(2) ifAbsent: []) = 'Two'].
	self should: [(tree atPath: #(2 4) ifAbsent: [#missing]) = #missing].
	self should: [(tree atPath: #(1 two three) ifAbsent: [#missing]) = #missing] raise: self defaultTestError.
	self should: [(tree atPath: #(3) ifAbsent: [#missing]) = #missing].