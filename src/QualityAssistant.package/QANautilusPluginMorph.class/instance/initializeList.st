initialization
initializeList
	listMorph := NonfocusablePluggableIconListMorph new
		model: self;
		getListSelector: #getCritiques;
		setIndexSelector:  #selected:;
		wrapSelector: #displayCritique:;
		hideHScrollBarIndefinitely: true;
		getIconSelector: #iconFor:at:;
		vResizing: #spaceFill; 
		hResizing: #spaceFill;
		color: self theme backgroundColor;
		backgroundColoringBlockOrSelector: [ :item :index |
			index odd
				ifTrue:  [ Smalltalk ui theme backgroundColor ]
				ifFalse: [ Smalltalk ui theme lightBackgroundColor ] ];
		yourself.
		