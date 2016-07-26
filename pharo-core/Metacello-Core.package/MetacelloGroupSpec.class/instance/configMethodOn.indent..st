printing
configMethodOn: aStream indent: indent

	aStream 
		tab: indent;
		nextPutAll: 'spec '; cr;
		tab: indent + 1;
		nextPutAll: 'name: ', self name printString, ';'.
	self configMethodBodyOn: aStream hasName: true cascading: false indent: indent + 1.
	aStream nextPut: $.