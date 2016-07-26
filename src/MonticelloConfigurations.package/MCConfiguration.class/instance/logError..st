private
logError: aString
	self log
		cr; nextPutAll: 'ERROR: ';
		nextPutAll: aString; cr;
		flush.
