testing
testRetrieveContents
	| fileReference contents url |
	SystemVersion current major < 3 ifTrue: [ self skip ].
	fileReference := ('file-{1}.txt' format: {(1000 atRandom)}) asFileReference.
	contents := 'Testing {1} and counting.{2}' format: {(1000 atRandom). (String crlf)}.
	fileReference ensureDelete.
	fileReference writeStreamDo: [ :stream | stream nextPutAll: contents ].
	url := fileReference asZnUrl.
	self assert: url isFile.
	self assert: url retrieveContents equals: contents.
	fileReference ensureDelete