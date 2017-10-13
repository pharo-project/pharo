tests - 32 bit
test32BitFormBlackShouldStayBlackAfterSave
	"Solve bug: https://pharo.fogbugz.com/f/cases/14619/Strange-behavior-of-TestRunner-Icon"

	| blackForm |
	blackForm := (Form extent: 10 @ 10) fillColor: Color black.
	self assert: blackForm colorsUsed first equals: Color black