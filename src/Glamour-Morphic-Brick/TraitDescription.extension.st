Extension { #name : #TraitDescription }

{ #category : #'*Glamour-Morphic-Brick' }
TraitDescription >> slotsDictionary [
	^ self isTrait 
		ifTrue: [ Dictionary new ]
		ifFalse: [ self layout slotsDictionary ]
]
