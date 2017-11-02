Extension { #name : #RBPatternBlockNode }

{ #category : #'*renraku' }
RBPatternBlockNode >> pattern [

	^ self source
		copyFrom: self sourceInterval first
		to: self sourceInterval last
]
