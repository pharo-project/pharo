slider-accessing
pagerModel: anAdapter
	
	pagerModel := anAdapter.
			
	pagerModel announcer when: GLMNumberOfVisiblePagesChanged send: #onResized to: self.
		
	pagerModel announcer when: GLMPagerScrollPositionChanged send: #onPositionChanged: to: self.
		
	self width: self computeWidth