Extension { #name : #EpRenameClassRefactoring }

{ #category : #'*EpiceaBrowsers' }
EpRenameClassRefactoring >> summaryOn: aWriteStream [
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: 'Rename ';
		nextPutAll: oldName;
		nextPutAll: ' into ';
		nextPutAll: newName.
]
