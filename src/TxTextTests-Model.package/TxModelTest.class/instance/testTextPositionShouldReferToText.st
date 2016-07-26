tests-default state
testTextPositionShouldReferToText


	self assert: text startPosition text == text.
	self assert: text endPosition text == text.
	
	self assert: text startPosition copy text == text.
