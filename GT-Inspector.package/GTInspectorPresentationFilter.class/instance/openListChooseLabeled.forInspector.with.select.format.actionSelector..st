private
openListChooseLabeled: sString forInspector: aGTInspector with: allElements select: selectedElements format: aBlockOrSelector actionSelector: aSymbol
	|treeModel|
	
	treeModel := MorphTreeModel new.
	treeModel
		 beCheckList;
		 rootItems: allElements;
		 wrapBlockOrSelector: aBlockOrSelector;
		 autoMultiSelection: true.
	 (treeModel openDialogWindowIn: World title: sString selectedtems: selectedElements) ifNotNil: [ :selection | 
		self perform: aSymbol with: selection with: aGTInspector ]