accessing
environmentsView
	^ environmentsView ifNil: [ 
		environmentsView := self instantiate: DropListPresenter. ]