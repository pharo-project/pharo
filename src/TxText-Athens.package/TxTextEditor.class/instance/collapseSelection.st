selection
collapseSelection
	"collapse the selection, moving selection start to selection end"
	self recordSelectionChange: [ 
		selection collapse.
	].
