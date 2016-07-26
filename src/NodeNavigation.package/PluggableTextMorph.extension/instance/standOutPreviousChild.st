*NodeNavigation
standOutPreviousChild
	self handleEdit: [
		NNavNavigation new 
			navigate: textMorph editor 
			direction: NNavDirectionPreviousBrother 
	]