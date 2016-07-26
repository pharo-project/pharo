initialize-release
beControl

	predicate := [:char | char asInteger < 32].
	negation := [:char | char asInteger >= 32]