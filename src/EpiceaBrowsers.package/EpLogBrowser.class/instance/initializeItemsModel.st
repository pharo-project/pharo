initialization
initializeItemsModel

	itemsModel 
		multiSelection: true;
		displayBlock: [ :item | item asMorph ];
		childrenBlock: [ :item | self childrenItemsOf: item ];
		whenSelectedItemsChanged: [ :selectedItems | 
			self refreshEntryContentModelWith: selectedItems.
			itemsModel menu: [ :aMenu | 
				self selection populateMenu: aMenu ] ].