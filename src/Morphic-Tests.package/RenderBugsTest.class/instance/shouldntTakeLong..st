private
shouldntTakeLong: aBlock
	"Check for infinite recursion. Test should finish in a reasonable time."

	^ self should: aBlock notTakeMoreThanMilliseconds: self long