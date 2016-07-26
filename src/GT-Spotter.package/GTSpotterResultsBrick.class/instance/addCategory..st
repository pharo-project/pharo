adding
addCategory: aCategoryBrick
	"Add a category brick as one of my children including category title.
	New category is added on position defined by spotter order of its processor.
	If there are two categories with the same order they are arranged using FIFO order.
	aCategoryBrick must not be nil.
	aCategoryBrick must not be already added
	aCategoryBrick must have at least one child item"
	self assert: aCategoryBrick isNotNil.
	self assert: aCategoryBrick hasOwner not.
	self assert: aCategoryBrick hasSubbricks.
	
	self band hasSubbricks
		ifFalse: [ 
			self band addBrickBack: aCategoryBrick titleBrick.
			self band addBrickBack: aCategoryBrick	]
		ifTrue: [ 
			|index|
			"initialize index with 2 because every second child in the list
			is category itself and all others are category titles"
			index := 2.
			[ index <= self band subbricksCount and: [ 
				(self band subbricks at: index) order <= aCategoryBrick order ] ]
			whileTrue: [ index := index + 2 ].
			index > self band subbricksCount
				"If all existing categories has order less than new one we just add to back"
				ifTrue: [
					self band addBrickBack: aCategoryBrick titleBrick.
					self band addBrickBack: aCategoryBrick ]
				"otherwise add after the last category with the same or smaller order"
				ifFalse: [ 
					self band addBrick: aCategoryBrick titleBrick after: (self band subbricks at: index).
					self band addBrick: aCategoryBrick titleBrick after: aCategoryBrick titleBrick ] ]
	