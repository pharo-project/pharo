private
buildLabel: text withIcon: icon
	
	^ (TabLabelItem tab: self)
		morph:	text asMorph;
		icon: (AlphaImageMorph new image: icon);
		yourself