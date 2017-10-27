tests - testing
testKeyForIdentity
	self assert: (nonEmptyDict keyForIdentity: 30) = #b.

	"The value 20 is associated to two different associations"
	self assert: (#(a c) includes: (nonEmptyDict keyForIdentity: self elementTwiceIn))