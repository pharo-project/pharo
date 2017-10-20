tests - valid context
testContextForNautilusItsANautilusContext
	| nautilus |
	
	nautilus := NautilusUI new.
	self assert: (SugsAbstractContext contextFor: nautilus) class equals: SugsNautilusContext.