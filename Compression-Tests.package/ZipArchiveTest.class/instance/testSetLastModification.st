tests
testSetLastModification

	| aDateAndTime member |
	aDateAndTime := '15 January, 2000T13:23:55'.
	member := zip addDeflateString: 'foo' as: 'bar'.
	member modifiedAt: aDateAndTime.
	self assert: member lastModTime equals: aDateAndTime.