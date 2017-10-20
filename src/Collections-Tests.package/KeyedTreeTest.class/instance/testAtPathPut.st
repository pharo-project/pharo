tests
testAtPathPut
	"Test the at path put method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1 two) put: #new) = #new].
	self should: [(tree atPath: #(1 two)) = #new].
	self should: [(tree atPath: #(1 three) put: (t2 := KeyedTree new)) = t2].
	self should: [(tree atPath: #(1 three $1) put: #anotherNew) = #anotherNew].
	self should: [(tree atPath: #(1 three $1)) = #anotherNew].
	self should: [tree atPath: #(2 4) put: [#new]] raise: self defaultTestError.
	self should: [(tree atPath: #(1 four one) put: #anotherNew) = #anotherNew].
	self should: [(tree atPath: #(1 four one)) = #anotherNew].