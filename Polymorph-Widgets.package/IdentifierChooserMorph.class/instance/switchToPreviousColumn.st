event handling
switchToPreviousColumn
	"Give the previous embedded menu keyboard focus."

	self switchToOtherColumn: [:prevIdx | 
		prevIdx =  1
			ifTrue: [self choiceMenus size]
			ifFalse: [prevIdx - 1]]
