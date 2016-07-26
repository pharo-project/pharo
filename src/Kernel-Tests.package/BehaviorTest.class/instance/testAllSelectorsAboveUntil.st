tests
testAllSelectorsAboveUntil
	"self debug: #testAllSelectorsAboveUntil"
	
	|sels |
	sels := Date allSelectorsAboveUntil: Object..
	self deny: (sels includes: #mmddyyyy). 
	self deny: (sels includes: #weekday).
	self assert: (sels includes: #at:).
	self deny: (sels includes: #cannotInterpret: )
	