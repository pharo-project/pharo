tests - 32 bit
test32BitOpaqueBlackIsTotallyBlack
	"The pixel value of a black at depth32 should really be black..."
	"At the time of this test, it returned 16rFF000001 ...."
	self assert: 16rFF000000 equals: (Color black pixelValueForDepth: 32)