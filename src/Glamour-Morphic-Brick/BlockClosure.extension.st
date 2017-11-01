Extension { #name : #BlockClosure }

{ #category : #'*Glamour-Morphic-Brick' }
BlockClosure >> brickValue: aBrick [

	^ self cull: aBrick
]

{ #category : #'*Glamour-Morphic-Brick' }
BlockClosure >> brickValue: anObject withEnoughArguments: aCollection [

	^ self valueWithEnoughArguments:  { anObject }, aCollection
]

{ #category : #'*Glamour-Morphic-Brick' }
BlockClosure >> phlowValue: anObject [

	^ self cull: anObject
]

{ #category : #'*Glamour-Morphic-Brick' }
BlockClosure >> phlowValue: anObject value: otherObject [

	^ self cull: anObject cull: otherObject
]

{ #category : #'*Glamour-Morphic-Brick' }
BlockClosure >> phlowValue: anObject withEnoughArguments: aCollection [

	^ self valueWithEnoughArguments:  aCollection
]
