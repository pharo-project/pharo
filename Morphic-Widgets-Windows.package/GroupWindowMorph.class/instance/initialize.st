initialization
initialize
	"Add the tab group with an inital workspace."

	super initialize.
	self changeProportionalLayout.
	self tabGroup: self newTabGroup.
	self tabGroup tabSelectorMorph addDependent: self.
	self
		dropEnabled: true;
		addMorph: self tabGroup fullFrame: LayoutFrame identity.
	self tabGroup color: Color transparent.
	