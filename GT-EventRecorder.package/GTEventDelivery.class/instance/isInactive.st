testing
isInactive
	^ mutex critical: [ 
		process isNil or: [
			process isTerminated or: [
				process isTerminating ] ] ]