visitor
visitCodeChange: aChange
	"Default behavior for any change."
		
	self error: 'Shouldn''t happen'. ^ {aChange}