helpers
assertStreamsInfoWithPrevious: beforeArray
	| afterArray |
	afterArray := self getStreamsInfoForRunningTest.
	self assert: beforeArray first equals: afterArray first.
	self assert: beforeArray second equals: afterArray second.