private-temporaries
handleBlockStack: aCollection with: aSHRange 
	| range blockStack |
	range := aSHRange.
	blockStack := aCollection.
	range isBlockStart 
		ifTrue: [ blockStack add: OrderedCollection new ]
		ifFalse: [ range isBlockEnd ifTrue: [ blockStack removeLast ] ]