accessing
segments
	"Return all the segments in the receiver"
	| out |
	out := Array new writeStream.
	self segmentsDo:[:seg| out nextPut: seg].
	^out contents