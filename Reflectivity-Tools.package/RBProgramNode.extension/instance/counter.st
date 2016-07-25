*Reflectivity-Tools-Watchpoints
counter
	
	self hasExecutionCounter ifFalse:[^nil].
	^(self beforeLinks detect: [ :link | link metaObject class = ExecutionCounter]) metaObject