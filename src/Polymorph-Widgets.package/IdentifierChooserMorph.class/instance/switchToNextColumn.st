event handling
switchToNextColumn
	"Give the next embedded menu keyboard focus."

	self switchToOtherColumn: [:prevIdx | 
		prevIdx =  self choiceMenus size
				ifTrue: [1]
				ifFalse: [prevIdx + 1]]
