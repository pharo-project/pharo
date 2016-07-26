vector functions
tangentAt: parameter
	| tan1 tan2 tan3 t1 t2 t3 |
	tan1 := via1 - start.
	tan2 := via2 - via1.
	tan3 := end - via2.
	t1 := (1.0 - parameter) squared.
	t2 := 2 * parameter * (1.0 - parameter).
	t3 := parameter squared.
	^(tan1 * t1) + (tan2 * t2) + (tan3 * t3)