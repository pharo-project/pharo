private
newButtonMorph
	"Answer a new button morph"

	^(CheckboxButtonMorph new
		target: self;
		actionSelector: #toggleSelected;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap)
		on: #mouseDown send: #buttonMouseDown: to: self