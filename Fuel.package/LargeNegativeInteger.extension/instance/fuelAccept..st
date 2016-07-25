*Fuel
fuelAccept: aGeneralMapper
	^ self >= -4294967295
		ifTrue: [ aGeneralMapper mapAndTraceByClusterName: self to: FLNegative32SmallIntegerCluster ]
		ifFalse: [ aGeneralMapper visitBytesObject: self]