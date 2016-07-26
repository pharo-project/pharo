instance creation
oldTrait: anOldTrait newTrait: aNewTrait

	^ self basicNew 
		initializeOldTrait: anOldTrait newTrait: aNewTrait;
		yourself