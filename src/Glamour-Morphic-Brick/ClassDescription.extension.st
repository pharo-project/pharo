Extension { #name : #ClassDescription }

{ #category : #'*Glamour-Morphic-Brick' }
ClassDescription >> slotsDictionary [
	^ self isTrait 
		ifTrue: [ Dictionary new ]
		ifFalse: [ self layout slotsDictionary ]
]
