attachments
midpoint
	"Answer the midpoint along my segments"
	| middle |
	middle := self totalLength.
	middle < 2 ifTrue: [ ^ self center ].
	middle := middle / 2.
	self lineSegmentsDo: [ :a :b | | dist |
		dist := (a dist: b).
		middle < dist
			ifTrue: [ ^(a + ((b - a) * (middle / dist))) asIntegerPoint ].
		middle := middle - dist.
	].
	self error: 'can''t happen'