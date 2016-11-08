private
treeMorphFor: tmpTreeModel and: aPresentation
	| tmpTreeMorph columns |
	tmpTreeMorph := self newTreeMorph.
	tmpTreeMorph
		makeLastColumnUnbounded;
		doubleClickSelector: #onDoubleClick;
		getMenuSelector: #menu:shifted:;
		keystrokeActionSelector: #keyStroke:from:;
		columnInset: 3;
		rowInset: 1;
		cornerStyle: tmpTreeMorph preferredCornerStyle;
		borderStyle: (BorderStyle inset width: 1);
		autoDeselection: aPresentation allowsDeselection;
		dragEnabled: tmpTreeModel dragEnabled;
		dropEnabled: tmpTreeModel dropEnabled;		
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		layoutFrame: (LayoutFrame identity).
	columns := aPresentation columns isEmpty 
		ifTrue: [ OrderedCollection with: (MorphTreeColumn new rowMorphGetSelector: #elementColumn)]
		ifFalse: [
			aPresentation columns collect: [:each | 
				GLMMorphTreeColumn new 
					startWidth: each width;
					glamourColumn: each; 
					headerButtonLabel: (aPresentation titleValueOfColumn: each)
					font: StandardFonts menuFont 
					target: tmpTreeModel
					actionSelector: #sortByColumn:
					arguments: {each}; 
					yourself ].
		].
	tmpTreeMorph
		preferedPaneColor: Smalltalk ui theme backgroundColor;	
		model: tmpTreeModel;
		columns: columns.
	aPresentation isMultiple 
		ifTrue: [tmpTreeMorph beMultiple] 
		ifFalse: [tmpTreeMorph beSingle].
	tmpTreeMorph chunkSize: aPresentation amountToShow.
	tmpTreeMorph pageSize: aPresentation amountToShow.
	tmpTreeMorph vShowScrollBar.
	tmpTreeMorph hShowScrollBar.
	^ tmpTreeMorph buildContents