tests
testAll
	guide := PostorderGuide for: self.
	guide show: (filesystem / 'alpha').
	self assertVisitedIs: #(
			'delta'
			'gamma'
			'beta'
			'zeta'
			'epsilon'
			'alpha'
		)