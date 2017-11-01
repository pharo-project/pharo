Extension { #name : #EpPropagateRefactoring }

{ #category : #'*EpiceaBrowsers' }
EpPropagateRefactoring >> summaryOn: aWriteStream [
	"Write a string summarizing myself."
	
	aWriteStream 
		nextPutAll: 'Propagate '.
		
	targetRefactoring summaryOn: aWriteStream
]
