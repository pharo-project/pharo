menu & halo
popUpContentsMenu: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu |
	menu := UIManager default newMenuIn: self for: self.
	self contentsMenu: menu.
	menu buildTitle: [ :titleMorph |
		titleMorph bigTitle: self contentsMenuTitle
	];
	popUpEvent: evt in: (self world ifNil: [self]).