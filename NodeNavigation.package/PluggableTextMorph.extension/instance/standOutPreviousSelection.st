*NodeNavigation
standOutPreviousSelection
	self handleEdit: [
		NNavNavigation new 
			navigate: textMorph editor 
			direction: NNavDirectionPreviousSelection  
	]