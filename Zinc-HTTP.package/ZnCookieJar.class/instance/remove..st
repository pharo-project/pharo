accessing
remove: aCookie
	self cookies remove: aCookie ifAbsent: [ nil ]