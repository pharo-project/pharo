helpers
waitProcessSuspend: aProcess

	self waitFor: [ aProcess isSuspended ]