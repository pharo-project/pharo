instance creation
newActionButtonRow
	"Answer a new row with the action buttons."
	
	^(self newRow: {
		self newUpButton.
		self newNewDirectoryButton.
		self newDeleteButton})
		listCentering: #bottomRight