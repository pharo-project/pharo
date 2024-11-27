Extension { #name : 'ASTPatternBlockNode' }

{ #category : '*Renraku' }
ASTPatternBlockNode >> pattern [

	^ self source
		copyFrom: self sourceInterval first
		to: self sourceInterval last
]
