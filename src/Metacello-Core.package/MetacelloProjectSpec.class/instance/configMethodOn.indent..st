printing
configMethodOn: aStream indent: indent

	aStream 
		tab: indent;
		nextPutAll: 'spec '; cr;
		tab: indent + 1;
		nextPutAll: 'name: ', self name printString, ';'.
	self configMethodBodyOn: aStream indent: indent.
	aStream nextPut: $.