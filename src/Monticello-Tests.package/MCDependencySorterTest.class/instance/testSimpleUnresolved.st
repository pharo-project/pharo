tests
testSimpleUnresolved
	self assertItems: #(
		(a () (z)))
	orderAs: #()
	withRequired: #(z)
	toLoad: #(a)
		