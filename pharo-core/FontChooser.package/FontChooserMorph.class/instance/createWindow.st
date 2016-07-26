interface building
createWindow
	"Create the package loader window."
	
	| buttonBar buttonBarOffset previewFrame buttonBarFrame |	
	buttonBar := self newRow: {
		self applyButton.
		self okButton.
		self cancelButton.
		self updateButton }.
	buttonBar cellInset: 8@0.
	buttonBarOffset := buttonBar minExtent y negated - (2 * ProportionalSplitterMorph splitterWidth).
	
	fontList := self newFontList borderWidth: 0.
	self 
		addMorph: fontList
		frame: (0.0 @ 0.0 corner: 0.4 @ 0.4).

	styleList := self newFontStyleList
		borderWidth: 0;
		yourself.
	self 
		addMorph: styleList
		frame: (0.4 @ 0.0 corner: 0.6 @ 0.4).

	pointSizeList := self newPointSizeList
		borderWidth: 0;
		yourself.
	self 
		addMorph: pointSizeList
		frame: (0.6 @ 0.0 corner: 0.75 @ 0.4).
		
	languageList := self newLanguageList
		borderWidth: 0;
		yourself.
	self 
		addMorph: languageList
		frame: (0.75 @ 0.0 corner: 1.0 @ 0.4).
	self languageIndex: 1.	
	
	previewFrame := (0 @ 0.4 corner: 1 @1) asLayoutFrame
		bottomOffset: buttonBarOffset
		yourself.
	self 
		addMorph: self fontPreviewPanel
		fullFrame: previewFrame. 

	buttonBarFrame := (0 @1 corner: 1@1) asLayoutFrame
		topOffset: buttonBarOffset;
		yourself.
	self 
		addMorph: buttonBar
		fullFrame: buttonBarFrame.