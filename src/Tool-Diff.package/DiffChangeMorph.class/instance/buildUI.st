initialization
buildUI

	|descriptionHeight|

	self
		diffMorph: self newDiffMorph;
		descriptionMorph: self newDescriptionMorph.
	descriptionHeight := self descriptionMorph minExtent y.
	self
		changeProportionalLayout;
		addMorph: self descriptionMorph
		fullFrame: ((0@0 corner: 1@0) asLayoutFrame bottomOffset: descriptionHeight);
		addMorph: self diffMorph
		fullFrame: (LayoutFrame identity topOffset: descriptionHeight).
	self
		extent: self initialExtent;
		updateDescriptionFillStyle: self paneColor