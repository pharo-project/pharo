factory
buildWidgetWith: aTabModel
	^ TabMorph new
		model: aTabModel;
		label: aTabModel label;
		icon: aTabModel icon;
		retrievingBlock: aTabModel retrievingBlock;
		morph: aTabModel contents;
		menu: aTabModel menu;
		closeable: aTabModel closeable;
		setActionsFrom: [ aTabModel actions ];
		yourself