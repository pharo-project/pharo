*Morphic-Base-Balloon
copyWith: aColor atRamp: rampIndex
	| ramp |
	ramp := colorRamp deepCopy.
	(ramp at: rampIndex) value: aColor.
	^(self class ramp: ramp) 
			origin: self origin;
			direction: self direction;
			normal: self normal;
			radial: self radial;
			yourself