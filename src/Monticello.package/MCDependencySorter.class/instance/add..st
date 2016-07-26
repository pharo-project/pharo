building
add: anItem
	| requirements |
	requirements := self unresolvedRequirementsFor: anItem.
	requirements isEmpty
		ifTrue: [self addToOrder: anItem]
		ifFalse: [self addRequirements: requirements for: anItem].
	^ anItem