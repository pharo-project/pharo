tests
testSimpleOrdering
	self assertItems: #((a (x) ())
								 (c () (y))
								 (b (y) (x)))
		orderAs: #(a b c)
		withRequired: #()
		toLoad: #()