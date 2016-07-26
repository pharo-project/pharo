*Slot
=> aVariable
	aVariable isBehavior
		ifTrue: [ ^ aVariable named: self].
	^ aVariable name: self;
		yourself