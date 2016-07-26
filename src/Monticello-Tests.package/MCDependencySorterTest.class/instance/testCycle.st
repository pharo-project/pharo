tests
testCycle
	self assertItems: #(
		(a (x) (y))
		(b (y) (x)))
	orderAs: #()
	withRequired: #()
	toLoad: #(a b)	