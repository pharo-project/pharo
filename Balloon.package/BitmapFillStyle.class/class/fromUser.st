instance creation
fromUser
	| fill |
	fill := self form: Form fromUser.
	fill origin: 0@0.
	fill direction: fill form width @ 0.
	fill normal: 0 @ fill form height.
	fill tileFlag: true. "So that we can fill arbitrary objects"
	^fill