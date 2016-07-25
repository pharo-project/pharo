morphic ui
order: anInteger
	"
	Set the order of the listed working copies
	anInteger represents the order of the mcworkingcopies in the MCBrowser. Its value is between 0 and the number of entries in MCFileRepositoryInspector >> orderSpecs
	"
	
	self class order: (order := anInteger).
	self changed: #workingCopyList; changed: #workingCopyTree