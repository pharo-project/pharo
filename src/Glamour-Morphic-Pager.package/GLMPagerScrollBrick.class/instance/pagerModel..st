accessing
pagerModel: aModel
	
	pagerModel := aModel.
	maxPanes := aModel numberOfVisiblePages.
	
	pagerModel announcer 
		when: GLMPagePushed send: #onPagePushed: to: self.
	
	pagerModel announcer 
		when: GLMPagePoped send: #onPagePoped: to: self.
	
	pagerModel announcer 
		when: GLMPagePopedAndReplaced send: #onPagePopedAndReplaced: to: self.
						
	pagerModel announcer 
		when: GLMPagerSynchronizeScrollAnnouncement send: #onSynchronizeScroll: to: self.
		
	pagerModel announcer  
		when: GLMPagerScrollPositionChanged send: #onPositionChanged: to: self.
		
	pagerModel announcer  
		when: GLMNumberOfVisiblePagesChanged send: #onPagesNumberChanged to: self.
		
	pagerModel announcer  
		when: GLMSelectedPageChanged send: #onSelectedPageChanged to: self.
		
	pagerModel announcer  
		when: GLMPagerPreviewUpdate send: #onPagePreviewUpdate: to: self.