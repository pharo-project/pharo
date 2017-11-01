Extension { #name : #Slot }

{ #category : #'*Glamour-Morphic-Brick' }
Slot >> phlowValue: anObject [
	
	^ anObject
]

{ #category : #'*Glamour-Morphic-Brick' }
Slot >> phlowValue: anObject value: otherObject [
	
	^ self write: otherObject to: anObject
]
