Extension { #name : #CompiledMethod }

{ #category : #'*Glamour-Morphic-Brick' }
CompiledMethod >> brickValue: anObject withEnoughArguments: aCollection [

	^ anObject withArgs: aCollection executeMethod: self
]

{ #category : #'*Glamour-Morphic-Brick' }
CompiledMethod >> phlowValue: anObject withEnoughArguments: aCollection [

	^ anObject withArgs: aCollection executeMethod: self
]
