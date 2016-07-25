initialization
initialize
	"Initialize the receiver."

	super initialize.
	listSelectionIndex := 0.
	enabled := true.
	list := #().
	"needs something to keep font"
	defaultContents := ' '.
	self
		useSelectionIndex: true;
		clipSubmorphs: true;
		layoutPolicy: RowLayout new;
		layoutInset: self layoutInsetToUse;
		cellPositioning: #center;
		listMorph: self newListMorph;
		contentMorph: self newContentMorph;
		buttonMorph: self newButtonMorph;
		fillStyle: self fillStyleToUse;
		borderStyle: self borderStyleToUse;
		addMorphBack: self contentMorph;
		addMorphBack: (self addDependent: self buttonMorph);
		on: #mouseDown send: #popList to: self;
		vResizing: #rigid;
		hResizing: #spaceFill;
		height: self font height + 10.
	self listMorph fillStyle: (self theme dropListNormalListFillStyleFor: self)