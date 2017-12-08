Extension { #name : #FiBitsLayout }

{ #category : #'*Shift-Changes-Ficus' }
FiBitsLayout >> layoutClassName [
	^ self isWordsLayout
		ifTrue: [ #WordLayout ]
		ifFalse: [ #ByteLayout ]
]
