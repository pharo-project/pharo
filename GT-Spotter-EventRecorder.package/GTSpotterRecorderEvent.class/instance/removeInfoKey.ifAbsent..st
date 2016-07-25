accessing
removeInfoKey: key ifAbsent: absentBlock
	info ifNil: [ ^ absentBlock value].
	^ info removeKey: key ifAbsent: absentBlock