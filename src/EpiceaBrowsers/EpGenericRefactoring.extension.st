Extension { #name : #EpGenericRefactoring }

{ #category : #'*EpiceaBrowsers' }
EpGenericRefactoring >> summaryOn: aWriteStream [
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: storeString
]
