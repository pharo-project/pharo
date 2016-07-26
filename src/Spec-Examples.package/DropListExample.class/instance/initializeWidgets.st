initialization
initializeWidgets
	uniformDropList := self newDropList.
	heterogeneousDropList := self newDropList.
	uniformDropList
		items:
			{morph1.
			morph2.
			morph3};
		displayBlock: [ :m | m color name capitalized , ' morph' ];
		iconHolder: [ :e | self iconNamed: #testGreenIcon ];
		whenSelectedItemChanged: [ :m | 
			container removeAllMorphs.
			container addMorph: m ].
	heterogeneousDropList
		addItemLabeled: 'Open workspace'
			do: [ Smalltalk tools workspace open ]
			icon: (self iconNamed: #smallOkIcon);
		addItemLabeled: 'Inspect current morph'
			do: [ uniformDropList selectedItem inspect ]
			icon: (self iconNamed: #testRedIcon).

	"If this is uncommented, it will fire the action of the first item, which is not what we want:
	heterogeneousDropList setSelectedIndex: 1.
	same for:
	heterogeneousDropList setIndex: 1"
	uniformDropList setIndex: 1.
	self setFocus