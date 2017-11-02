Extension { #name : #EpRenameMethodRefactoring }

{ #category : #'*EpiceaBrowsers' }
EpRenameMethodRefactoring >> summaryOn: aWriteStream [
	"Write a string summarizing myself."
	
	aWriteStream
		nextPutAll: 'Rename ';
		nextPutAll: oldSelector printString;
		nextPutAll: ' into ';
		nextPutAll: newSelector printString.
]
