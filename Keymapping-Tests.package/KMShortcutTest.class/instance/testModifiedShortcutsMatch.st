tests
testModifiedShortcutsMatch
	|a oneShift oneCommand oneCommandShift|
	
	a := self eventKey: $a ctrl: true.
	oneShift := self eventKey: $1 shift: true.
	oneCommand := self eventKey: $1 command: true.
	oneCommandShift := self eventKey: $1 alt: false ctrl: false command: true shift: true.
	
	self assert: ($a ctrl matches: {a}).
	self assert: ($1 shift matches: {oneShift}).
	self assert: ($1 command matches: {oneCommand}).
	self assert: ($1 command shift matches: {oneCommandShift}).
	
	self deny: ($a ctrl matches: {self eventKey: $a command: true}).
	
	self deny: ($i command shift matches: {self eventKey: $i command: true}).
	self assert: ($i ctrl shift matches: {self eventKey: $i alt: false ctrl: true command: false shift: true}).
	self deny: ($i ctrl matches: {self eventKey: $i alt: false ctrl: true command: false shift: true}).
	
	self assert: ($a ctrl matchesCompletely: {a}).
	self assert: ($1 shift matchesCompletely: {oneShift}).
	self assert: ($1 command matchesCompletely: {oneCommand}).
	self assert: ($1 command shift matchesCompletely: {oneCommandShift}).