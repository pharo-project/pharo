accessing
infoAt: key ifPresent: presentBlock ifAbsent: absentBlock
	info ifNil: [ ^ absentBlock value].
	^ info at: key ifPresent: presentBlock ifAbsent: absentBlock