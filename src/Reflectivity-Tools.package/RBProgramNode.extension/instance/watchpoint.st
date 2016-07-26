*Reflectivity-Tools-Watchpoints
watchpoint
	
	self hasWatchpoint ifFalse:[^nil].
	^(self afterLinks detect: [ :link | link metaObject class = Watchpoint ]) metaObject