accessing
infoAt: key ifAbsent: absentBlock
	info ifNil: [ ^ absentBlock value].
	^ info at: key ifAbsent: absentBlock