tests-positionDiff
testDifferenceEquivalentToMovingRight
	| start  end |
	self newText: 'ABC', String cr, 'DEF'.

	start := text startPosition.
	end := text endPosition.
	
	self assert: end -- start = -7.
	self assert: start -- end = 7.

	self assert: (end + (-7) ) isAtStart.
	self assert: (start + (7) ) isAtEnd.
				
