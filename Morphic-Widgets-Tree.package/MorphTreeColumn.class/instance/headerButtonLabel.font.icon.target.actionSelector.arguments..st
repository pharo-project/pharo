column drawing
headerButtonLabel: aLabel font: aFont icon: anIconForm target: aReceiver actionSelector: aSelector arguments: aCollection
	self headerButton.
	self header label: aLabel font: aFont.
	self header icon: (ImageMorph new form: anIconForm).
	self header actionSelector: aSelector.
	self header target: aReceiver.
	self header arguments: aCollection