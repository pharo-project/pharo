accessing
infoAt: key put: value
	info ifNil: [ info := Dictionary new ].
	^ info at: key put: value