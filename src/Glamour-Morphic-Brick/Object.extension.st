Extension { #name : #Object }

{ #category : #'*Glamour-Morphic-Brick' }
Object >> asBrick [

	^ GLMMorphBrick on: self asMorph
]

{ #category : #'*Glamour-Morphic-Brick' }
Object >> brickAlert [

	GLMAlertBrick alert: self
]

{ #category : #'*Glamour-Morphic-Brick' }
Object >> brickValue: aBrick [

	^ self
]

{ #category : #'*Glamour-Morphic-Brick' }
Object >> brickValue: anObject withEnoughArguments: aCollection [

	^ self
]

{ #category : #'*Glamour-Morphic-Brick' }
Object >> phlowValue: anObject [

	^ self
]

{ #category : #'*Glamour-Morphic-Brick' }
Object >> phlowValue: anObject value: otherObject [

	^ self
]

{ #category : #'*Glamour-Morphic-Brick' }
Object >> phlowValue: anObject withEnoughArguments: aCollection [

	^ self
]
