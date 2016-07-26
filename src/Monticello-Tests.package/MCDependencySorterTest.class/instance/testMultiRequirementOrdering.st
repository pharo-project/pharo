tests
testMultiRequirementOrdering
	self assertItems: #(
		(a (x) (z))
		(b (y) ())
		(c (z) ())
		(d () (x y z)))
		orderAs: #(b c a d)
		withRequired: #()
		toLoad: #()