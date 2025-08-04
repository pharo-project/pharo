Extension { #name : 'OCPatternBlockNode' }

{ #category : '*Renraku' }
OCPatternBlockNode >> pattern [

	^ self source
		copyFrom: self sourceInterval first
		to: self sourceInterval last
]
