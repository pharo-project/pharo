helpers
waitProcessTermination: aProcess

	self waitFor: [ aProcess isTerminated ]