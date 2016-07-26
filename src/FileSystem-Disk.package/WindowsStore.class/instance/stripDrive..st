converting
stripDrive: pathElements
	pathElements ifNotEmpty: [ pathElements at: 1 put: ( ($: split: pathElements first) last)  ]