Extension { #name : #EpUnknownRefactoring }

{ #category : #'*EpiceaBrowsers' }
EpUnknownRefactoring >> summaryOn: aWriteStream [
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: printString
]
