testing
testCannotLoad
	| definition |
	definition := self mockMethod: #kjahs87 class: 'NoSuchClass' source: 'kjahs87 ^self' meta: false.
	self should: [definition load] raise: Error.
	self assert: (navigation allImplementorsOf: #kjahs87) isEmpty