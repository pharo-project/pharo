Extension { #name : #TClassDescription }

{ #category : #'*Glamour-Morphic-Brick' }
TClassDescription >> slotsDictionary [
	^ self isTrait 
		ifTrue: [ Dictionary new ]
		ifFalse: [ self layout slotsDictionary ]
]
