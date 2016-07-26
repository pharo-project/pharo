tests
testAllSelectorsAbove
	"self debug: #testAllSelectorsAbove"
	
	|sels |
	sels := Date allSelectorsAbove.
	self deny: (sels includes: #mmddyyyy). 
	self deny: (sels includes: #weekday).
	self assert: (sels includes: #at:).
	self assert: (sels includes: #cannotInterpret: )
	