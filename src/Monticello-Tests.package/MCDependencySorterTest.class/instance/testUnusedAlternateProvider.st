tests
testUnusedAlternateProvider
	self assertItems: #(
		(a (x) (z))
		(b () (x))
		(c (x) ()))
	orderAs: #(c b)
	withRequired: #(z)
	toLoad: #(a)	