tests
testAll
	guide := PreorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'alpha'
			'beta'
			'delta'
			'gamma'
			'epsilon'
			'zeta'
		)