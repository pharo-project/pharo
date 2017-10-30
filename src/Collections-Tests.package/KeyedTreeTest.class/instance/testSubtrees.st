tests
testSubtrees
	"Test the subtrees method for success and failure modes."

	|t1 t2 t3 t4|
	t1 := KeyedTree new
		at: 1 put: (t2 := KeyedTree new
					at: 1 put: '1-1';
					at: 2 put: '1-2';
					at: 3 put: (t3 := KeyedTree new
								at: 1 put: '1-3-1';
								at: 2 put: '1-3-2';
								yourself);
					yourself);
		at: 2 put: '2';
		at: 3 put: (t4 := KeyedTree new
					at: 1 put: '1-3-1';
					at: 2 put: '1-3-2';
					yourself);
		yourself.
	self should: [t1 subtrees = {t2. t4}].
	self should: [(t1 at: 1) subtrees = {t3}].