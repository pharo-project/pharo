Extension { #name : #BlockClosure }

{ #category : #'*SortFunctions-Core-controlling' }
BlockClosure >> asSortFunction [
	"Return a SortFunction around the receiver. If the receiver is a 2 arg block, it is assumed it will do the collation directly itself, returning -1, 0, or 1. If the receiver is a one arg block, it will be evaluated for each a and b and of the sort input, and the result of sending <=> to those will be used"
	
	self numArgs = 1 ifTrue: [^SortByPropertyFunction computedBy: self].
	self numArgs = 2 ifTrue: [^CollatorBlockFunction usingBlock: self].

	self error: 'Cant be converted to sort function. It should has one or two args'	
]

{ #category : #'*SortFunctions-Core-controlling' }
BlockClosure >> ascending [
	"Return a SortFunction around the receiver. If the receiver is a 2 arg block, it is assumed it will do the collation directly itself, returning -1, 0, or 1. If the receiver is a one arg block, it will be evaluated for each a and b and of the sort input, and the result of sending <=> to those will be used."

	^self asSortFunction beAscending
]

{ #category : #'*SortFunctions-Core-controlling' }
BlockClosure >> descending [
	"Opposite direction as ascending."

	^self asSortFunction beDescending
]
