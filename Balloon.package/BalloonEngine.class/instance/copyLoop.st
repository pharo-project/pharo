copying
copyLoop
	"This is the basic rendering loop using as little primitive support as possible."
	| finished edge fill |
	edge := BalloonEdgeData new.
	fill := BalloonFillData new.
	self primInitializeProcessing. "Initialize the GE for processing"
	[self primFinishedProcessing] whileFalse:[
		"Step 1: Process the edges in the global edge table that will be added in this step"
		[finished := self primNextGlobalEdgeEntryInto: edge.
		finished] whileFalse:[
			edge source: (externals at: edge index).
			edge stepToFirstScanLine.
			self primAddActiveEdgeTableEntryFrom: edge].

		"Step 2: Scan the active edge table"
		[finished := self primNextFillEntryInto: fill.
		finished] whileFalse:[
			fill source: (externals at: fill index).
			"Compute the new fill"
			fill computeFill.
			"And mix it in the out buffer"
			self primMergeFill: fill destForm bits from: fill].

		"Step 3: Display the current span buffer if necessary"
		self primDisplaySpanBuffer.

		"Step 4: Advance and resort the active edge table"
		[finished := self primNextActiveEdgeEntryInto: edge.
		finished] whileFalse:[
			"If the index is zero then the edge has been handled by the GE"
			edge source: (externals at: edge index).
			edge stepToNextScanLine.
			self primChangeActiveEdgeTableEntryFrom: edge].
	].
	self primGetTimes: Times.
	self primGetCounts: Counts.
	self primGetBezierStats: BezierStats.