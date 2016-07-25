accessing
stderr
	(stderr isNil or: [ stderr closed ])
		ifTrue: [ stderr := self createStderr ].
	^ stderr 