Extension { #name : #Symbol }

{ #category : #'*Glamour-Morphic-Brick' }
Symbol >> brickValue: aBrick [

	^ self cull: aBrick
]

{ #category : #'*Glamour-Morphic-Brick' }
Symbol >> brickValue: anObject withEnoughArguments: aCollection [

	^ anObject perform: self withEnoughArguments: aCollection
]

{ #category : #'*Glamour-Morphic-Brick' }
Symbol >> phlowValue: anObject [

	^ self cull: anObject
]

{ #category : #'*Glamour-Morphic-Brick' }
Symbol >> phlowValue: anObject value: otherObject [

	^ anObject perform: self withEnoughArguments: { otherObject }
]

{ #category : #'*Glamour-Morphic-Brick' }
Symbol >> phlowValue: anObject withEnoughArguments: aCollection [

	^ anObject perform: self withEnoughArguments: aCollection
]
