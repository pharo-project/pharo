model access
changeModelSelection: anInteger
	"Change the model's selected item index to be anInteger."

	setIndexSelector ifNotNil:
		[model perform: setIndexSelector with: anInteger].
	self isMultipleSelection
		ifTrue: [ 
			self listSelectionAt: self lastNonZeroIndex put: false.
			self listSelectionAt: anInteger put: true ]