accessing
fileName
	^ (self class nameForVer: info name base: base name), '.', self writerClass extension