user interface
window
	| sliceInfoRow buttonsRow okBtn cancelBtn issueNumberEntry issueSummaryEntry tree treeMorph |
	window := StandardWindow new model: self.
	window title: ' Slice Maker' translated.
	issueNumberEntry := self issueNumberEntryOn: window.
	issueSummaryEntry := self issueTitleOn: window.
	sliceInfoRow := PanelMorph new
		changeTableLayout;
		cellInset: 10 @ 4;
		layoutInset: 4 @ 4;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	sliceInfoRow
		addAllMorphs:
			{issueNumberEntry.
			(window newLabel: '-').
			issueSummaryEntry}.
	buttonsRow := PanelMorph new
		changeTableLayout;
		cellInset: 10 @ 4;
		layoutInset: 4 @ 4;
		listDirection: #rightToLeft;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
	okBtn := window newOKButtonFor: self getEnabled: #okEnabled.
	cancelBtn := window newCancelButtonFor: self.
	okBtn isDefault: true.
	self info addDependent: okBtn.
	buttonsRow
		addAllMorphs:
			{cancelBtn.
			okBtn}.
	window addMorph: sliceInfoRow fullFrame: ((0 @ 0 corner: 1 @ 0) asLayoutFrame bottomOffset: sliceInfoRow height).
	tree := MorphTreeModel new
		rootItems: self rootItems;
		beCheckList;
		autoMultiSelection: true;
		wrapBlockOrSelector: #packageNameWithStar;
		headerLabel: 'Dirty Packages to be included in the slice';
		yourself.
	tree onSelectionChangeSend: #selectionChanged: to: self.
	treeMorph := tree defaultTreeMorph
		buildContents;
		vResizing: #spaceFill;
		yourself.	"selectAll; "	"hResizing: #spaceFill;"
	window
		addMorph: treeMorph
		fullFrame:
			(LayoutFrame identity
				topOffset: sliceInfoRow height;
				bottomOffset: buttonsRow height negated).
	window addMorph: buttonsRow fullFrame: ((0 @ 1 corner: 1 @ 1) asLayoutFrame topOffset: buttonsRow height negated).
	^ window