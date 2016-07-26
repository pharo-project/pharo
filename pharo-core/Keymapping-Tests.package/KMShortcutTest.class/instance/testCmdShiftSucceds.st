tests
testCmdShiftSucceds
	^ self assert: ($1 shift ctrl matches: {self eventKey: $1 alt: false ctrl: true command: false shift: true})