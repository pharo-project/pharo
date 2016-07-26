tests
testCascadingUnresolved
	self assertItems: #(
		(a (x) (z))
		(b () (x))
		(c () ()))
	orderAs: #(c)
	withRequired: #(z)
	toLoad: #(a b)	