accessing
collectionClasses
	^ (Array with: Set with: Bag) , (self sequenceableClasses)