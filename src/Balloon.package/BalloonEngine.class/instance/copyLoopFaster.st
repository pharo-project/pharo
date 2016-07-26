copying
copyLoopFaster
	"This is a copy loop drawing one scan line at a time"
	| edge fill reason |
	edge := BalloonEdgeData new.
	fill := BalloonFillData new.
	[self primFinishedProcessing] whileFalse:[
		reason := self primRenderScanline: edge with: fill.
		"reason ~= 0 means there has been a problem"
		reason = 0 ifFalse:[
			self processStopReason: reason edge: edge fill: fill.
		].
	].
	self primGetTimes: Times.
	self primGetCounts: Counts.
	self primGetBezierStats: BezierStats.