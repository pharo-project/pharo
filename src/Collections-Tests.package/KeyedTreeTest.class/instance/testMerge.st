tests
testMerge
	"Test the merge method for success and failure modes."

	|t1 t2 m|
	t1 := KeyedTree new
		at: 1 put: (KeyedTree new
					at: 1 put: '1-1';
					at: 2 put: '1-2';
					at: 3 put: (KeyedTree new
								at: 1 put: '1-3-1';
								at: 2 put: '1-3-2';
								yourself);
					yourself);
		at: 2 put: '2';
		yourself.
	t2 := KeyedTree new
		at: 1 put: (KeyedTree new
					at: 1 put: (KeyedTree new
								at: 1 put: '1-1-1';
								at: 2 put: '1-1-2';
								yourself);
					at: 2 put: '1-2*';
					yourself);
		at: 3 put: '3';
		yourself.
	m := t1 merge: t2.
	self should: [(m at: 2) = '2'].
	self should: [(m at: 3) = '3'].
	self should: [(m atPath: #(1 2)) = '1-2*'].
	self should: [(m atPath: #(1 1 1)) = '1-1-1'].
	self should: [(m atPath: #(1 1 2)) = '1-1-2'].
	self should: [(m atPath: #(1 3 1)) = '1-3-1'].
	self should: [(m atPath: #(1 3 2)) = '1-3-2'].