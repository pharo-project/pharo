vector functions
tangentAtMid
	| tan1 tan2 tan3 |
	tan1 := via1 - start.
	tan2 := via2 - via1.
	tan3 := end - via2.
	^(tan1 + (2*tan2) + tan3) * 0.25
