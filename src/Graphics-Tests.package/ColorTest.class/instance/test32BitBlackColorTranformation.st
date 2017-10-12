tests - 32 bit
test32BitBlackColorTranformation
	"Solve bug: https://pharo.fogbugz.com/f/cases/14619/Strange-behavior-of-TestRunner-Icon"

	self assert: ((Color black pixelWordForDepth: 32) asColorOfDepth: 32) equals: Color black