Extension { #name : 'Integer' }

{ #category : '*Metacello-Core' }
Integer >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticIntegerLessThanSelf: self
]
