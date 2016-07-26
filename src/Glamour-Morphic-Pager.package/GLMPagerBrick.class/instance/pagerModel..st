pager-accessing
pagerModel: aModel

	pagerModel := aModel.
	
	self pagerModel announcer when: GLMPagePushed send: #onPagePushed to: self.
	self pagerModel announcer when: GLMPagePoped send: #onPagePoped to: self.
	
	self initializeShortcuts.
	
	self scrollBrick: self newScrollBrick.
	self pagerBar: self newPagerBar