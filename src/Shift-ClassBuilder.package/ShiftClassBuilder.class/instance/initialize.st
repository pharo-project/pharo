initialization
initialize
	super initialize.
	
	superclassName := #Object.
	builderEnhancer := self class defaultBuildEnhancer new.

	layoutDefinition := ShLayoutDefinition new.
	layoutDefinition builder: self.

	extensibleProperties := Dictionary new.
	
	changeComparers := OrderedCollection new.
	changes := Set new.
	
	self addChangeComparer: ShSharedPoolChangeDetector.
	self addChangeComparer: ShSharedVariablesChangeDetector.
	self addChangeComparer: ShSlotChangeDetector.
	self addChangeComparer: ShLayoutChangeDetector.
	self addChangeComparer: ShBitLayoutChangeDetector.
	self addChangeComparer: ShClassSlotChangeDetector.
	
	builderEnhancer initializeBuilder: self.