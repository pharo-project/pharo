accessing
selectedInfo
	^ self infos at: self selection ifAbsent: [nil]