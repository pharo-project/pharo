initialization
initialize
	"Initialize the receiver."

	| ppCheckbox |
	super initialize.
	showOnlyDestination := false.
	showOnlySource := false.
	showOptions := true.
	self
		srcMorph: self newSrcMorph;
		joinMorph: self newJoinMorph;
		dstMorph: self newDstMorph;
		scrollbarMorph: self newScrollbarMorph;
		mapMorph: self newMapMorph;
		changeProportionalLayout.
	self prettyPrint: TextDiffBuilder diffsWithPrettyPrint.
	ppCheckbox := self newPrettyPrintCheckboxMorph.
	optionsPanel := self newPanel
		addMorph: ((self newRow: {ppCheckbox}) listCentering: #bottomRight);
		color: self defaultColor.
	optionsPanel vResizing: #shrinkWrap.
	optionsPanel extent: optionsPanel minExtent.
	self addMorphsWithOptions.
	self srcMorph scrollPane announcer 
		when: RubVerticalScrolled send: #verticalScroll: to: self;
		when: RubHorizontalScrolled send: #horizontalSourceScroll: to: self.
	self dstMorph scrollPane announcer 
		when: RubVerticalScrolled send: #verticalScroll: to: self;
		when: RubHorizontalScrolled send: #horizontalDestScroll: to: self.
	self
		linkSubmorphsToSplitters;
		extent: self initialExtent