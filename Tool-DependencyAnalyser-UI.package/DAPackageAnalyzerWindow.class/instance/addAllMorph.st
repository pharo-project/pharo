adding
addAllMorph
	| windowNautilus  windowTreeModel |
	windowNautilus := self buildNautilusWindow.	
	windowTreeModel := self buildMorphWindowForTreeModel.
	packageAnalyzerTreeModel packageTreeNautilusUI: self nautilusUI.
	self
		addMorph: windowNautilus frame: (0.4 @ 0 corner: 1 @ 1);
		addMorph:  (self panelMorphFrom: windowTreeModel) frame: (0 @ 0 corner: 0.4 @ 1).