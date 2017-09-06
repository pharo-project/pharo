initialization
initialize
	super initialize.
	self
		replace: '``@Boolean ifTrue: [`variable := ``@true] ifFalse: [`variable := ``@false]'
		with: '`variable := ``@Boolean ifTrue: [``@true] ifFalse: [``@false]';
		replace: '``@Boolean ifFalse: [`variable := ``@true] ifTrue: [`variable := ``@false]'
		with: '`variable := ``@Boolean ifFalse: [``@true] ifTrue: [``@false]'