accessing
tail
	| nn |
	nn := self.
	[ nn next notNil ] whileTrue: [ nn := nn next ].
	^ nn