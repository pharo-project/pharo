actions
expand
	"If the last element of the visible stack is selected, expand the stack,
	if more elements are available."
	
	self list selectedItem ifNil: [ ^ self ]. 
	
	self list listItems ifNotNil: [ :aList |
		(self list selectedIndex = self list listSize and: [
			self list selectedItem sender notNil ]) ifTrue: [ 
				self expandBy: self class defaultStackSize ] ]
	