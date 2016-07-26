notifying
notifyPagePushed: aMorph scrollTo: isScroll smooth: isSmooth
	
	self notifyAll: (GLMPagePushed new morph: aMorph).
	
	isScroll ifTrue: [
		self scrollingDelay > 0
			ifTrue: [ self scrollToRightTask execute: { isSmooth } ]
			ifFalse: [ self scrollToRight: isSmooth ] ]