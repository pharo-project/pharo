Extension { #name : #EpCompositeRefactoring }

{ #category : #'*EpiceaBrowsers' }
EpCompositeRefactoring >> summaryOn: aWriteStream [
	"Write a string summarizing myself."
	
	childrenRefactorings
		do: [:each | each summaryOn: aWriteStream ]
		separatedBy: [ aWriteStream nextPutAll: ', ' ]
]
