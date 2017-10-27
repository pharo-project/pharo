initialization
initializeWidgets

	methodsListPresenter := self instantiate: FastTablePresenter.
	classesListPresenter := self instantiate: FastTablePresenter.
	changesListPresenter := self instantiate: FastTablePresenter.
	prettyButton := self newCheckBox.
	diffButton := self newCheckBox.
	textPresenter := self newCode.
	 
	self setFocus.
	
	methodsListPresenter menu: [:menu :shifted | self messageMenu: menu shifted: shifted ].
	changesListPresenter menu: [:aMenu :shifted | self changeSetMenu: aMenu shifted: shifted  ].
	classesListPresenter menu: [:aMenu :shifted | self classMenu: aMenu shifted: shifted ].

	changesListPresenter items: self model allChanges.
	changesListPresenter displayBlock: [:item | item name ].
	textPresenter aboutToStyle: true.
	