tests - 32 bit
test32BitTranslucentPixelValueKeepsRGB
	"The pixel value of a translucent color at depth32 should keep the RGB component irrespective of alpha.
	At the time of this test, setting an alpha of zero made the entire pixel 0 irrespective of depth..."
	self assert: 16rFFFFFF equals: ((Color white alpha: 0) pixelValueForDepth: 32)