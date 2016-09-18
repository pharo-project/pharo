initialization
initializeWidgets
	tree := self newTree
		multiSelection: true;
		on: $b meta
			do: [ self selectedItemFromTree browseInOtherNautilus ];
		columns: {(TreeColumnModel new displayBlock: [ :node | node label ])};
		yourself.
	buttonRefresh := self newButton
		icon: (self iconNamed: #refreshIcon);
		help: 'Refresh the TreeModel';
		label: 'Refresh';
		yourself.
	packageTreeNautilus := Nautilus new