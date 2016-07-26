operations
updateFont
	"Update the label font."

	self tabs do: [:t | t font: self font].
	self updateTabs