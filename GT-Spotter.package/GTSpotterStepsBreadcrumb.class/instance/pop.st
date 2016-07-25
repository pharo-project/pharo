adding/removing
pop
	
	self band layoutAfter: [
		self band removeBrick: self items removeLast.
		self items size = 1 ifTrue: [
			self items first labelBrick paddingLeft: 0 ].
		self items isEmpty ifFalse: [ self popSeparator ].
		self items do: [ :b | b wrappedBounds markWidthDirty ] ].
	
	"optimization. if last item was popped we need to update root to reposition all brick,
	otherwise only I need to be updated"
	self items size = 0
		ifTrue: [ self notifyBreadcrumbVisibilityChanged ]
		ifFalse: [
			self items last showTriangle.
			self scrollToTheLastOne.
			self changed ]