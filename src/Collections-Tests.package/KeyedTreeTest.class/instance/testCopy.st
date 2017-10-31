tests
testCopy
	"Test the copy method for success and failure modes."

	|c tree t2 t3|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					at: #four put: (t3 := KeyedTree new);
					yourself);
		at: 2 put: 'Two';
		yourself.
	c := tree copy.
	self should: [c = tree].
	self shouldnt: [c == tree].
	self should: [(c at: 1) = t2].
	self shouldnt: [(c at: 1) == t2].
	self should: [(c atPath: #(1 four)) = t3].
	self shouldnt: [(c atPath: #(1 four)) == t3].