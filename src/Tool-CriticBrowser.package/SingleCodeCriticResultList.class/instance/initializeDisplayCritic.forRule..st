initialization
initializeDisplayCritic: aCritic forRule: aRule
	
	| criticRepresentation | 
	criticRepresentation := (self formatCritic: aCritic) asMorph.
	^ (cache isFalsePositive: aCritic forRule: aRule) 
			ifTrue: [ criticRepresentation color: self falsePositiveColor ]
			ifFalse: [ (cache isToDo: aCritic forRule: aRule)
						ifTrue: [ criticRepresentation color: self toDoColor ]
						ifFalse: [ criticRepresentation color: self defaultColor ]]