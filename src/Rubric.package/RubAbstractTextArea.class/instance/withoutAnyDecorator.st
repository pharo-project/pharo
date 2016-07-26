accessing decorators
withoutAnyDecorator
	[ self paragraph key notNil ] whileTrue: [ self withoutDecorator: paragraph ]