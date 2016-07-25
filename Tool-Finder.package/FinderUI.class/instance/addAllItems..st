items creation
addAllItems: aWindow
	| toolbar height  btnFont  btnToolbar cst |
	toolbar := self buildSearchModeToolBar: aWindow.
	height := toolbar minExtent y.
	cst := 15.
	
	aWindow 
		addMorph: toolbar
		fullFrame: ( (0 @ 0 corner: 1 @ 0) asLayoutFrame bottomOffset: height).
	aWindow
		addMorph: self buildPackagesTree buildContents
		fullFrame: ((0@0 corner: 1@0.58) asLayoutFrame topOffset: height).
	btnFont := StandardFonts buttonFont.
	btnToolbar := self buildBrowseToolBar: aWindow.
	aWindow
		addMorph: btnToolbar
		fullFrame: ((0@0.58 corner: 1@0.58) asLayoutFrame bottomOffset: (btnFont height +cst)).
	aWindow 
		addMorph: self sourceTextArea
		fullFrame: ((0@0.58 corner: 1@1) asLayoutFrame topOffset: btnFont height +cst)
