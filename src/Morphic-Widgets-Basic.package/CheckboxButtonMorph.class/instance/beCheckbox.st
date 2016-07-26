protocol
beCheckbox
	"Change the images and square the border
	to be a checkbox."

	self
		isRadioButton: false;
		onImage: self theme checkboxMarkerForm;
		cornerStyle: (self theme checkboxCornerStyleFor: self);
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse