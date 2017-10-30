tests
testAtPath
	"Test the at path method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1)) = t2].
	self should: [(tree atPath: #(1 two)) = 'One-Two'].
	self should: [(tree atPath: #(1 three)) = 'One-Three'].
	self should: [(tree atPath: #(2)) = 'Two'].
	self should: [tree atPath: #(2 4)] raise: self defaultTestError.
	self should: [tree atPath: #(1 two three)] raise: self defaultTestError.
	self should: [tree atPath: #(3)] raise: self defaultTestError.