tests
testExtraProvisions
	self assertItems:
		#((a (x) (z))
		(b () (x)))
	orderAs: #(a b)
	withRequired: #()
	toLoad: #()	
	extraProvisions: #(x z)