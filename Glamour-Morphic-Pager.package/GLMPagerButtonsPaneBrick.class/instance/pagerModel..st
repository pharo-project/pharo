buttons-accessing
pagerModel: aModel
	
	pagerModel := aModel.
		
	self pagerModel announcer when: GLMPageSwitched send: #onPageSwitched to: self.
	self pagerModel announcer when: GLMPagerBoundsChanged send: #onScrollBoundsChanged: to: self.
	self pagerModel announcer when: GLMFocusedPageChanged send: #onFocusedPageChanged to: self.
		
	self updateButtons