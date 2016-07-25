private
logWarning: aString
	self log
		cr; nextPutAll: 'WARNING: ';
		nextPutAll: aString; cr;
		flush.
