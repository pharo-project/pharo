asserting
skipUnless: aBooleanOrBlock
	" If the assumption in aBooleanOrBlock is not true, abandon the running test
	and mark it as passed. "
	aBooleanOrBlock value ifFalse:
			[ TestSkip signal: 'Assumption in #skipUnless: failed' ]