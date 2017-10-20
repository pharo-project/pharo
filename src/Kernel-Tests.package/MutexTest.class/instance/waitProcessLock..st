helpers
waitProcessLock: aProcess

	self waitFor: [ aProcess suspendingList notEmpty ]