computing
intStepToNextScanLineAt: yValue in: edgeTableEntry
	"Scaled integer version of forward differencing"
	[maxSteps >= 0 and:[yValue * 256 > lastY]] whileTrue:[
		self validateIntegerRange.
		lastX := lastX + ((fwDx + 16r8000) // 16r10000).
		lastY := lastY + ((fwDy + 16r8000) // 16r10000).
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
		maxSteps := maxSteps - 1.
	].
	edgeTableEntry xValue: lastX // 256.
	edgeTableEntry zValue: 0.