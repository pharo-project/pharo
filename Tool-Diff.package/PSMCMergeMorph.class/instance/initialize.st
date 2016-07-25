initialization
initialize
	"Initialize the receiver."

	|buttons buttonsHeight|
	super initialize.
	buttons := self newButtonsMorph.
	buttonsHeight := buttons minExtent y.
	self
		merged: false;
		patchMorph: self newPatchMorph;
		codeMorph: self newCodeMorph;
		changeProportionalLayout;
		addMorph: self patchMorph
		fullFrame: (0@0 corner: 1@0.6) asLayoutFrame ;
		addMorph: self codeMorph
		fullFrame: ((0@0.6 corner: 1@1) asLayoutFrame bottomOffset: buttonsHeight negated);
		addMorph: self newButtonsMorph
		fullFrame: ((0@1 corner: 1@1) asLayoutFrame topOffset: buttonsHeight negated);
		addPaneSplitters