initialization
initialize
	super initialize.
	
	categories := Dictionary new.
	self band hSpaceFill.
	self announcer when: GLMBrickScrollPositionChanged send: #onScrolled to: self