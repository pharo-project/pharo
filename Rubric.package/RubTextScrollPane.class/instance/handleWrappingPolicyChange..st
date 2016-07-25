text area protocol
handleWrappingPolicyChange: aBlock
	aBlock value.
	self wrapped
		ifTrue: [ 
			self fitScrollTargetWidth.
			self updateScrollbars ]