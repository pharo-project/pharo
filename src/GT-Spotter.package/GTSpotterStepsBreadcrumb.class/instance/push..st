adding/removing
push: aBrick

	self themer spotterThemer breadcrumbItemStyleFor: aBrick.
	
	self band layoutAfter: [
		self items isEmpty ifFalse: [
			self items last hideTriangle.
			self addSeparator ].
		self addItem: aBrick ].
	
	"optimization. if first item was added we need to update root to reposition all bricks,
	otherwise only I need to be updated"
	self items size = 1
		ifTrue: [ self notifyBreadcrumbVisibilityChanged ]
		ifFalse: [
			self scrollToTheLastOne.
			self changed ].
	