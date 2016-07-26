initialization
initialize
	"Initialize the receiver."

	super initialize.
	self removeMorph: self contentMorph;
		removeMorph: self tabSelectorMorph;  
		toolbarMorph: self newToolbarMorph;
		headerMorph: self newHeaderMorph;
		addMorph: self headerMorph;
		addMorph: self contentMorph.
	
"	self
		borderWidth: 0;
		changeTableLayout;
		cellPositioning: #topLeft;
		cellInset: 0 @ -1;
		reverseTableCells: true;
		pageMorphs: OrderedCollection new;
		tabSelectorMorph: self newTabSelectorMorph;
		contentMorph: self newContentMorph;
		addMorph: self tabSelectorMorph;
		addMorph: self contentMorph.
	self tabSelectorMorph addDependent: self"