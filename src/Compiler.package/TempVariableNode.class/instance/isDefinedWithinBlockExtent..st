code generation (closures)
isDefinedWithinBlockExtent: anInterval
	^anInterval rangeIncludes: definingScope actualScope blockExtent first