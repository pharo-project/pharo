Extension { #name : #ByteSymbol }

{ #category : #'*Keymapping-Core' }
ByteSymbol >> asKmCategoryIn: aKmRepository [
	^aKmRepository categoryForName: self
]
