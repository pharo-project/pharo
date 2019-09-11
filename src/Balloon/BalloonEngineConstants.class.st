"
Constants for the baloon engine
"
Class {
	#name : #BalloonEngineConstants,
	#superclass : #SharedPool,
	#classVars : [
		'BEAaLevelIndex',
		'BEBalloonEngineSize',
		'BEBitBltIndex',
		'BEClipRectIndex',
		'BEColorTransformIndex',
		'BEDeferredIndex',
		'BEDestOffsetIndex',
		'BEEdgeTransformIndex',
		'BEExternalsIndex',
		'BEFormsIndex',
		'BEPostFlushNeededIndex',
		'BESpanIndex',
		'BEWorkBufferIndex',
		'ETBalloonEdgeDataSize',
		'ETIndexIndex',
		'ETLinesIndex',
		'ETSourceIndex',
		'ETXValueIndex',
		'ETYValueIndex',
		'ETZValueIndex',
		'FTBalloonFillDataSize',
		'FTDestFormIndex',
		'FTIndexIndex',
		'FTMaxXIndex',
		'FTMinXIndex',
		'FTSourceIndex',
		'FTYValueIndex',
		'GBBaseSize',
		'GBBitmapDepth',
		'GBBitmapHeight',
		'GBBitmapRaster',
		'GBBitmapSize',
		'GBBitmapWidth',
		'GBColormapOffset',
		'GBColormapSize',
		'GBEndX',
		'GBEndY',
		'GBFinalX',
		'GBMBaseSize',
		'GBTileFlag',
		'GBUpdateDDX',
		'GBUpdateDDY',
		'GBUpdateDX',
		'GBUpdateDY',
		'GBUpdateData',
		'GBUpdateX',
		'GBUpdateY',
		'GBViaX',
		'GBViaY',
		'GBWideEntry',
		'GBWideExit',
		'GBWideExtent',
		'GBWideFill',
		'GBWideSize',
		'GBWideUpdateData',
		'GBWideWidth',
		'GEBaseEdgeSize',
		'GEBaseFillSize',
		'GEEdgeClipFlag',
		'GEEdgeFillsInvalid',
		'GEFillIndexLeft',
		'GEFillIndexRight',
		'GENumLines',
		'GEObjectIndex',
		'GEObjectLength',
		'GEObjectType',
		'GEObjectUnused',
		'GEPrimitiveBezier',
		'GEPrimitiveClippedBitmapFill',
		'GEPrimitiveEdge',
		'GEPrimitiveEdgeMask',
		'GEPrimitiveFill',
		'GEPrimitiveFillMask',
		'GEPrimitiveLine',
		'GEPrimitiveLinearGradientFill',
		'GEPrimitiveRadialGradientFill',
		'GEPrimitiveRepeatedBitmapFill',
		'GEPrimitiveTypeMask',
		'GEPrimitiveUnknown',
		'GEPrimitiveWide',
		'GEPrimitiveWideBezier',
		'GEPrimitiveWideEdge',
		'GEPrimitiveWideLine',
		'GEPrimitiveWideMask',
		'GEStateAddingFromGET',
		'GEStateBlitBuffer',
		'GEStateCompleted',
		'GEStateScanningAET',
		'GEStateUnlocked',
		'GEStateUpdateEdges',
		'GEStateWaitingChange',
		'GEStateWaitingForEdge',
		'GEStateWaitingForFill',
		'GEXValue',
		'GEYValue',
		'GEZValue',
		'GErrorAETEntry',
		'GErrorBadState',
		'GErrorFillEntry',
		'GErrorGETEntry',
		'GErrorNeedFlush',
		'GErrorNoMoreSpace',
		'GFDirectionX',
		'GFDirectionY',
		'GFNormalX',
		'GFNormalY',
		'GFOriginX',
		'GFOriginY',
		'GFRampLength',
		'GFRampOffset',
		'GGBaseSize',
		'GLBaseSize',
		'GLEndX',
		'GLEndY',
		'GLError',
		'GLErrorAdjDown',
		'GLErrorAdjUp',
		'GLWideEntry',
		'GLWideExit',
		'GLWideExtent',
		'GLWideFill',
		'GLWideSize',
		'GLWideWidth',
		'GLXDirection',
		'GLXIncrement',
		'GLYDirection',
		'GWAAColorMask',
		'GWAAColorShift',
		'GWAAHalfPixel',
		'GWAALevel',
		'GWAAScanMask',
		'GWAAShift',
		'GWAETStart',
		'GWAETUsed',
		'GWBezierHeightSubdivisions',
		'GWBezierLineConversions',
		'GWBezierMonotonSubdivisions',
		'GWBezierOverflowSubdivisions',
		'GWBufferTop',
		'GWClearSpanBuffer',
		'GWClipMaxX',
		'GWClipMaxY',
		'GWClipMinX',
		'GWClipMinY',
		'GWColorTransform',
		'GWCountAddAETEntry',
		'GWCountChangeAETEntry',
		'GWCountDisplaySpan',
		'GWCountFinishTest',
		'GWCountInitializing',
		'GWCountMergeFill',
		'GWCountNextAETEntry',
		'GWCountNextFillEntry',
		'GWCountNextGETEntry',
		'GWCurrentY',
		'GWCurrentZ',
		'GWDestOffsetX',
		'GWDestOffsetY',
		'GWEdgeTransform',
		'GWFillMaxX',
		'GWFillMaxY',
		'GWFillMinX',
		'GWFillMinY',
		'GWFillOffsetX',
		'GWFillOffsetY',
		'GWGETStart',
		'GWGETUsed',
		'GWHasClipShapes',
		'GWHasColorTransform',
		'GWHasEdgeTransform',
		'GWHeaderSize',
		'GWLastExportedEdge',
		'GWLastExportedFill',
		'GWLastExportedLeftX',
		'GWLastExportedRightX',
		'GWMagicIndex',
		'GWMagicNumber',
		'GWMinimalSize',
		'GWNeedsFlush',
		'GWObjStart',
		'GWObjUsed',
		'GWPoint1',
		'GWPoint2',
		'GWPoint3',
		'GWPoint4',
		'GWPointListFirst',
		'GWSize',
		'GWSpanEnd',
		'GWSpanEndAA',
		'GWSpanSize',
		'GWSpanStart',
		'GWState',
		'GWStopReason',
		'GWTimeAddAETEntry',
		'GWTimeChangeAETEntry',
		'GWTimeDisplaySpan',
		'GWTimeFinishTest',
		'GWTimeInitializing',
		'GWTimeMergeFill',
		'GWTimeNextAETEntry',
		'GWTimeNextFillEntry',
		'GWTimeNextGETEntry'
	],
	#category : #'Balloon-Engine'
}

{ #category : #'private - initialization' }
BalloonEngineConstants class >> initEdgeConstants [
	"Initialize the edge constants"

	"Edge primitive types"
	GEPrimitiveEdge := 2.			"External edge - not handled by the GE"
	GEPrimitiveWideEdge := 3.	"Wide external edge"
	GEPrimitiveLine := 4.			"Straight line"
	GEPrimitiveWideLine := 5.	"Wide line"
	GEPrimitiveBezier := 6.		"Quadratic bezier curve"
	GEPrimitiveWideBezier := 7.	"Wide bezier curve"

	"Special flags"
	GEPrimitiveWide := 16r01.			"Flag determining a wide primitive"
	GEPrimitiveWideMask := 16rFE.		"Mask for clearing the wide flag"
	GEEdgeFillsInvalid := 16r10000. 	"Flag determining if left/right fills of an edge are invalid"
	GEEdgeClipFlag := 16r20000.			"Flag determining if this is a clip edge"

	"General edge state constants"
	GEXValue := 4.						"Current raster x"
	GEYValue := 5.						"Current raster y"
	GEZValue := 6.						"Current raster z"
	GENumLines := 7.					"Number of scan lines remaining"
	GEFillIndexLeft := 8.				"Left fill index"
	GEFillIndexRight := 9.			"Right fill index"
	GEBaseEdgeSize := 10.				"Basic size of each edge"

	"General fill state constants"
	GEBaseFillSize := 4.				"Basic size of each fill"

	"General Line state constants"
	GLXDirection := 10.				"Direction of edge (1: left-to-right; -1: right-to-left)"
	GLYDirection := 11.				"Direction of edge (1: top-to-bottom; -1: bottom-to-top)"
	GLXIncrement := 12.				"Increment at each scan line"
	GLError := 13.						"Current error"
	GLErrorAdjUp := 14.				"Error to add at each scan line"
	GLErrorAdjDown := 15.				"Error to subtract on roll-over"
			"Note: The following entries are only needed before the incremental
			state is computed. They are therefore aliased to the error values above"
	GLEndX := 14.						"End X of line"
	GLEndY := 15.						"End Y of line"
	GLBaseSize := 16.					"Basic size of each line"

	"Additional stuff for wide lines"
	GLWideFill := 16.					"Current fill of line"
	GLWideWidth := 17.					"Current width of line"
	GLWideEntry := 18.					"Initial steps"
	GLWideExit := 19.					"Final steps"
	GLWideExtent := 20.				"Target width"
	GLWideSize := 21.					"Size of wide lines"

	"General Bezier state constants"
	GBUpdateData := 10.				"Incremental update data for beziers"
	GBUpdateX := 0.						"Last computed X value (24.8)"
	GBUpdateY := 1.						"Last computed Y value (24.8)"
	GBUpdateDX := 2.					"Delta X forward difference step (8.24)"
	GBUpdateDY := 3.					"Delta Y forward difference step (8.24)"
	GBUpdateDDX := 4.					"Delta DX forward difference step (8.24)"
	GBUpdateDDY := 5.					"Delta DY forward difference step (8.24)"
		"Note: The following four entries are only needed before the incremental
			state is computed. They are therefore aliased to the incremental values above"
	GBViaX := 12.						"via x"
	GBViaY := 13.						"via y"
	GBEndX := 14.						"end x"
	GBEndY := 15.						"end y"
	GBBaseSize := 16.					"Basic size of each bezier.
                                   Note: MUST be greater or equal to the size of lines"

	"Additional stuff for wide beziers"
	GBWideFill := 16.					"Current fill of line"
	GBWideWidth := 17.					"Current width of line"
	GBWideEntry := 18.					"Initial steps"
	GBWideExit := 19.					"Final steps"
	GBWideExtent := 20.				"Target extent"
	GBFinalX := 21.						"Final X value"
	GBWideUpdateData := 22.			"Update data for second curve"
	GBWideSize := 28					"Size of wide beziers"


]

{ #category : #'private - initialization' }
BalloonEngineConstants class >> initFillConstants [
	"Initialize the fill constants"

	"Fill primitive types"
	GEPrimitiveFill := 16r100.
	GEPrimitiveLinearGradientFill := 16r200.
	GEPrimitiveRadialGradientFill := 16r300.
	GEPrimitiveClippedBitmapFill := 16r400.
	GEPrimitiveRepeatedBitmapFill := 16r500.

	"General fill state constants"
	GEBaseFillSize := 4.			"Basic size of each fill"

	"Oriented fill constants"
	GFOriginX := 4.					"X origin of fill"
	GFOriginY := 5.					"Y origin of fill"
	GFDirectionX := 6.				"X direction of fill"
	GFDirectionY := 7.				"Y direction of fill"
	GFNormalX := 8.					"X normal of fill"
	GFNormalY := 9.					"Y normal of fill"

	"Gradient fill constants"
	GFRampLength := 10.			"Length of following color ramp"
	GFRampOffset := 12.			"Offset of first ramp entry"
	GGBaseSize := 12.

	"Bitmap fill constants"
	GBBitmapWidth := 10.			"Width of bitmap"
	GBBitmapHeight := 11.			"Height of bitmap"
	GBBitmapDepth := 12.			"Depth of bitmap"
	GBBitmapSize := 13.			"Size of bitmap words"
	GBBitmapRaster := 14.			"Size of raster line"
	GBColormapSize := 15.			"Size of colormap, if any"
	GBTileFlag := 16.				"True if the bitmap is tiled"
	GBColormapOffset := 18.		"Offset of colormap, if any"
	GBMBaseSize := 18				"Basic size of bitmap fill"
]

{ #category : #'private - initialization' }
BalloonEngineConstants class >> initPrimitiveConstants [
	"Initialize the primitive constants"

	"Primitive type constants"
	GEPrimitiveUnknown := 0.
	GEPrimitiveEdgeMask := 16rFF.
	GEPrimitiveFillMask := 16rFF00.
	GEPrimitiveTypeMask := 16rFFFF.

	"General state constants (Note: could be compressed later)"
	GEObjectType := 0.				"Type of object"
	GEObjectLength := 1.			"Length of object"
	GEObjectIndex := 2.			"Index into external objects"
	GEObjectUnused := 3			"Currently unused"


]

{ #category : #'private - initialization' }
BalloonEngineConstants class >> initStateConstants [
	"Initialize the state constants"

	GEStateUnlocked := 0.				"Buffer is unlocked and can be modified as wanted"
	GEStateAddingFromGET := 1.		"Adding edges from the GET"
	GEStateWaitingForEdge := 2.		"Waiting for edges added to GET"
	GEStateScanningAET := 3.			"Scanning the active edge table"
	GEStateWaitingForFill := 4.		"Waiting for a fill to mix in during AET scan"
	GEStateBlitBuffer := 5.			"Blt the current scan line"
	GEStateUpdateEdges := 6.			"Update edges to next scan line"
	GEStateWaitingChange := 7.		"Waiting for a changed edge"
	GEStateCompleted := 8.			"Rendering completed"

	"Error constants"
	GErrorNoMoreSpace := 1.			"No more space in collection"
	GErrorBadState := 2.				"Tried to call a primitive while engine in bad state"
	GErrorNeedFlush := 3.				"Tried to call a primitive that requires flushing before"

	"Incremental error constants"
	GErrorGETEntry := 4.				"Unknown entry in GET"
	GErrorFillEntry := 5.				"Unknown FILL encountered"
	GErrorAETEntry := 6				"Unknown entry in AET"

]

{ #category : #'private - initialization' }
BalloonEngineConstants class >> initWorkBufferConstants [
	"Initialize the work buffer constants"

	"General work buffer constants"
	GWMagicNumber := 16r416E6469.	"Magic number"
	GWHeaderSize := 128.				"Size of header"
	GWMinimalSize := 256.				"Minimal size of work buffer"

	"Header entries"
	GWMagicIndex := 0.					"Index of magic number"
	GWSize := 1.							"Size of full buffer"
	GWState := 2.						"Current state (e.g., locked or not."
	"Buffer entries"
	GWObjStart := 8.					"objStart"
	GWObjUsed := 9.						"objUsed"
	GWBufferTop := 10.					"wbTop"
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
	GWSpanStart := 32.					"spStart"
	GWSpanSize := 33.					"spSize"
	GWSpanEnd := 34.					"spEnd"
	GWSpanEndAA := 35.					"spEndAA"

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
	GWLastExportedEdge := 65.		"last exported edge"
	GWLastExportedFill := 66.		"last exported fill"
	GWLastExportedLeftX := 67.		"last exported leftX"
	GWLastExportedRightX := 68.		"last exported rightX"
	GWClearSpanBuffer := 69.			"Do we have to clear the span buffer?"
	GWPointListFirst := 70.			"First point list in buffer"

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
	GWBezierHeightSubdivisions := 109.	"# of subdivisions due to excessive height"
	GWBezierOverflowSubdivisions := 110.	"# of subdivisions due to possible int overflow"
	GWBezierLineConversions := 111.		"# of beziers converted to lines"

	GWHasClipShapes := 112.		"True if the engine contains clip shapes"
	GWCurrentZ := 113				"Current z value of primitives"

]

{ #category : #initialization }
BalloonEngineConstants class >> initialize [
	"BalloonEngineConstants initialize"
	self initStateConstants.
	self initWorkBufferConstants.
	self initPrimitiveConstants.
	self initEdgeConstants.
	self initFillConstants.
	self initializeInstVarNames: BalloonEngine prefixedBy: 'BE'.
	self initializeInstVarNames: BalloonEdgeData prefixedBy: 'ET'.
	self initializeInstVarNames: BalloonFillData prefixedBy: 'FT'.
]

{ #category : #'private - initialization' }
BalloonEngineConstants class >> initializeInstVarNames: aClass prefixedBy: aString [

	| token |
	aClass instVarNames doWithIndex:[:instVarName :index| | value aToken |
		aToken := (aString, instVarName first asUppercase asString, (instVarName copyFrom: 2 to: instVarName size),'Index') asSymbol.
		value := index - 1.
		(self bindingOf: aToken) ifNil:[ self addClassVarNamed: aToken ].
		(self bindingOf: aToken) value: value.
	].
	token := (aString, aClass name,'Size') asSymbol.
	(self bindingOf: token) ifNil: [ self addClassVarNamed: token ].
	(self bindingOf: token) value: aClass instSize
]
