accessing-selection
setModelSelection: aSelectionInterval
	"try to set the selection in the model"
	setSelectionSelector
		ifNotNil: [ self model perform: setSelectionSelector with: aSelectionInterval]