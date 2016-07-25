morph
newButtonMorph
	"Answer a new button morph"

	| instance |
	instance := TickSelectionCheckboxButtonMorph new
		target: self;
		actionSelector: #toggleSelected;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.
		
	instance index: index.
	^ instance