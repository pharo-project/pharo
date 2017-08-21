*Glamour-Morphic-Brick
slotsDictionary
	^ self isTrait 
		ifTrue: [ Dictionary new ]
		ifFalse: [ self layout slotsDictionary ]