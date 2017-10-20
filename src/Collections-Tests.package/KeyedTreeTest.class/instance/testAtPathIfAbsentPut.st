tests
testAtPathIfAbsentPut
	"Test the at path if absent put method for success and failure modes."

	|tree t2|
	tree := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: #two put: 'One-Two';
					at: #three put: 'One-Three';
					yourself);
		at: 2 put: 'Two';
		yourself.
	self should: [(tree atPath: #(1) ifAbsentPut: [#new]) = t2].
	self should: [(tree atPath: #(1 two) ifAbsentPut: [#new]) = 'One-Two'].
	self should: [(tree atPath: #(1 three) ifAbsentPut: [#new]) = 'One-Three'].
	self should: [(tree atPath: #(2) ifAbsentPut: [#new]) = 'Two'].
	self should: [tree atPath: #(2 4) ifAbsentPut: [#new]] raise: self defaultTestError.
	self should: [tree atPath: #(1 two three) ifAbsentPut: [#new]] raise: self defaultTestError.
	self should: [(tree atPath: #(1 four one) ifAbsentPut: [#anotherNew]) = #anotherNew].
	self should: [(tree atPath: #(1 four one)) = #anotherNew].
	self should: [(tree atPath: #(3) ifAbsentPut: [#yetAnotherNew]) = #yetAnotherNew].
	self should: [(tree atPath: #(3)) = #yetAnotherNew].