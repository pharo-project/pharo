pool definition
initWorkBufferConstants
	"Initialize the work buffer constants"

	"General work buffer constants"
	GWMagicNumber := 16r416E6469.	"Magic number"
	GWHeaderSize := 128.				"Size of header"
	GWMinimalSize := 256.				"Minimal size of work buffer"

	"Header entries"
	GWMagicIndex := 0.				"Index of magic number"
	GWSize := 1.						"Size of full buffer"
	GWState := 2.						"Current state (e.g., locked or not."
	"Buffer entries"
	GWObjStart := 8.					"objStart"
	GWObjUsed := 9.					"objUsed"
	GWBufferTop := 10.				"wbTop"
	GWGETStart := 11.					"getStart"
	GWGETUsed := 12.					"getUsed"
	GWAETStart := 13.					"aetStart"
	GWAETUsed := 14.					"aetUsed"

	"Transform entries"
	GWHasEdgeTransform := 16.		"True if we have an edge transformation"
	GWHasColorTransform := 17.		"True if we have a color transformation"
	GWEdgeTransform := 18.			"2x3 edge transformation"
	GWColorTransform := 24.			"8 word RGBA color transformation"

	"Span entries"
	GWSpanStart := 32.				"spStart"
	GWSpanSize := 33.					"spSize"
	GWSpanEnd := 34.					"spEnd"
	GWSpanEndAA := 35.				"spEndAA"

	"Bounds entries"
	GWFillMinX := 36.					"fillMinX"
	GWFillMaxX := 37.					"fillMaxX"
	GWFillMinY := 38.					"fillMinY"
	GWFillMaxY := 39.					"fillMaxY"
	GWFillOffsetX := 40.				"fillOffsetX"
	GWFillOffsetY := 41.				"fillOffsetY"
	GWClipMinX := 42.
	GWClipMaxX := 43.
	GWClipMinY := 44.
	GWClipMaxY := 45.
	GWDestOffsetX := 46.
	GWDestOffsetY := 47.

	"AA entries"
	GWAALevel := 48.					"aaLevel"
	GWAAShift := 49.					"aaShift"
	GWAAColorShift := 50.				"aaColorShift"
	GWAAColorMask := 51.				"aaColorMask"
	GWAAScanMask := 52.				"aaScanMask"
	GWAAHalfPixel := 53.				"aaHalfPixel"

	"Misc entries"
	GWNeedsFlush := 63.				"True if the engine may need a flush"
	GWStopReason := 64.				"stopReason"
	GWLastExportedEdge := 65.			"last exported edge"
	GWLastExportedFill := 66.			"last exported fill"
	GWLastExportedLeftX := 67.			"last exported leftX"
	GWLastExportedRightX := 68.		"last exported rightX"
	GWClearSpanBuffer := 69.			"Do we have to clear the span buffer?"
	GWPointListFirst := 70.				"First point list in buffer"

	GWPoint1 := 80.
	GWPoint2 := 82.
	GWPoint3 := 84.
	GWPoint4 := 86.

	GWCurrentY := 88.

	"Profile stats"
	GWTimeInitializing := 90.
	GWCountInitializing := 91.
	GWTimeFinishTest := 92.
	GWCountFinishTest := 93.
	GWTimeNextGETEntry := 94.
	GWCountNextGETEntry := 95.
	GWTimeAddAETEntry := 96.
	GWCountAddAETEntry := 97.
	GWTimeNextFillEntry := 98.
	GWCountNextFillEntry := 99.
	GWTimeMergeFill := 100.
	GWCountMergeFill := 101.
	GWTimeDisplaySpan := 102.
	GWCountDisplaySpan := 103.
	GWTimeNextAETEntry := 104.
	GWCountNextAETEntry := 105.
	GWTimeChangeAETEntry := 106.
	GWCountChangeAETEntry := 107.

	"Bezier stats"
	GWBezierMonotonSubdivisions := 108. 	"# of subdivision due to non-monoton beziers"
	GWBezierHeightSubdivisions := 109.		"# of subdivisions due to excessive height"
	GWBezierOverflowSubdivisions := 110.	"# of subdivisions due to possible int overflow"
	GWBezierLineConversions := 111.		"# of beziers converted to lines"

	GWHasClipShapes := 112.		"True if the engine contains clip shapes"
	GWCurrentZ := 113.			"Current z value of primitives"
