initialization
initializePresenter
	super initializePresenter.
	self  addButton
		action: [ | requirement |
			requirement := addItemBlock value.
			requirement ifNotNil: [ self list: (self list listItems copyWith: requirement) ] ].
	self removeButton action: [ self list: (list listItems copyWithoutIndex: list selectedIndex) ].
	self topButton action: [ self moveElementAt: list selectedIndex to: 1 ].
	self bottomButton action: [ self moveElementAt: list selectedIndex to: list listItems size ].
	self upButton action: [ self moveElementAt: list selectedIndex to: list selectedIndex - 1 ].
	self downButton action: [ self moveElementAt: list selectedIndex to: list selectedIndex + 1 ]