initialization
initializePresenter
	message acceptBlock: [ :text | 
		text ifNotEmpty: [ self model commitWithMessage: text asString. ] ].
	commitButton action: [ message accept ].
	refreshButton action: [ self model refresh ]