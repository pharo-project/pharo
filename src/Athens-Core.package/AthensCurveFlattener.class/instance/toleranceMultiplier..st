accessing
toleranceMultiplier: aToleranceMultiplier

	"	
	A tolerance multiplier is a number .. which
		if = 1 , the default error tolerance is used,
		if > 1 you will get more coarse approximation
		if < 1 you will get more finer approximation
	
	"
	
	lengthTolerance := aToleranceMultiplier.
	lengthToleranceSquared := lengthTolerance squared.
	
