private
morph: aMorph withTitleOf: aPresentation	
	| tabs |
	tabs := LazyTabGroupMorph new.
	tabs
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		cornerStyle: (self theme tabGroupCornerStyleIn: nil);
		font: self theme labelFont.
	self flag: 'add a regular page instead of a lazy one'.
	self flag: 'handle the PresentationChanged announcement to update the label in the morph. 
	The problem is that at the moment I do not know how to link to the label in the tabSelectorMorph'.
	tabs 
		addLazyPage: aMorph
		label: (self titleOrIconOf: aPresentation in: tabs)
		toolbar: (self renderToolbarForPresentation: aPresentation inMorph: aMorph)
		action: (self labelActionBrickFor: aPresentation).
	tabs selectedPageIndex: 1.
	^ tabs