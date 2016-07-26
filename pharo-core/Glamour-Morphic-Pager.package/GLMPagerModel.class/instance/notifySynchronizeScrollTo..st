notifying
notifySynchronizeScrollTo: aValue 
	
	self notifyAll: (GLMPagerSynchronizeScrollAnnouncement new value: aValue).