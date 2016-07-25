initialization
initializeWidgets

	repositories := self newList.
	workingCopies := self newList.
		
	workingCopies sortingBlock: self defaultOrderingBlock.
	workingCopies displayBlock: [ :item | item description ].
	workingCopies menu: [ :aMenu | self workingCopiesMenu: aMenu ].
		
	repositories displayBlock: [ :item | item description ].
	repositories items: self allRepositories.

	"so that when we tab we go from working copies to repositories"
	self focusOrder
		add: workingCopies;
		add: repositories