list
variableFieldsToShow
	"Answers the indexes of the variable fields of the object to show.
	Shorten the list for very long collection (cf limit1 and limit2)"

	|bSize|
	
	bSize := self objectVariableSize.
	
	^ bSize <= (self limit1 + self limit2)
			ifTrue: [1 to: bSize]
			ifFalse: [(1 to: self limit1) , (bSize - self limit2 + 1 to: bSize)].