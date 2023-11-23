Extension { #name : 'RBPatternBlockNode' }

{ #category : '*Renraku' }
RBPatternBlockNode >> pattern [

	^ self source
		copyFrom: self sourceInterval first
		to: self sourceInterval last
]
