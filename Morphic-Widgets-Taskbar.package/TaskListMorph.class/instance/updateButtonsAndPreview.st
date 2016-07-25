private
updateButtonsAndPreview
	"Update the buttons and the preview.."
	
	self
		updateTaskButtons;
		updatePreview.
	self world ifNotNil: 
		[self center: self world center]