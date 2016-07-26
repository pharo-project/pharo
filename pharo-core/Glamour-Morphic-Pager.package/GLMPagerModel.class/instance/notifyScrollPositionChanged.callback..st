notifying
notifyScrollPositionChanged: isSmooth callback: aBlock

	self notifyAll: (GLMPagerScrollPositionChanged new smooth: isSmooth; callback: aBlock; yourself).