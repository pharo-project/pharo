computing
floatStepToNextScanLineAt: yValue in: edgeTableEntry
	"Float version of forward differencing"
	[yValue asFloat > lastY] whileTrue:[
		self assert: [fwDx < -50.0 or:[fwDx > 50.0]].
		self assert: [fwDy < -50.0 or:[fwDy > 50.0]].
		self assert: [fwDDx < -50.0 or:[fwDDx > 50.0]].
		self assert: [fwDDy < -50.0 or:[fwDDy > 50.0]].
		lastY := lastY + fwDy.
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
	].
	edgeTableEntry xValue: lastX asInteger.
	edgeTableEntry zValue: 0.