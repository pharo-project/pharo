instance initalization
returnInteger: n
	self signedLongAt: self typeOffset put: 0.
	self signedLongAt: self valueOffset put: n asInteger