instance creation
openOn: aClass
	| window |
	self rootClass: aClass.
	window := StandardWindow new model: self.
	window title: aClass name, ' hierarchy'.
	window 
		addMorph: self treeMorph
		fullFrame: LayoutFrame identity.						
	window themeChanged.
	window openInWorld.
	^ window