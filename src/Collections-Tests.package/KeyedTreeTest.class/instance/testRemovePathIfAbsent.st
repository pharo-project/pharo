tests
testRemovePathIfAbsent
	"Test the remove path if absent method for success and failure modes."

	|tree|
	tree := KeyedTree new
		at: 1 put: (KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree removePath: #(4) ifAbsent: [#none]) = #none].
	self should: [(tree removePath: #(1 2 3 4) ifAbsent: [#none]) = #none].
	self should: [(tree removePath: #(1 two) ifAbsent: [#none]) = 'One-Two'].
	self should: [(tree atPath: #(1 two) ifAbsent: []) = nil].
	self should: [(tree removePath: #(2) ifAbsent: [#none]) = 'Two'].
	self should: [(tree atPath: #(2) ifAbsent: []) = nil].