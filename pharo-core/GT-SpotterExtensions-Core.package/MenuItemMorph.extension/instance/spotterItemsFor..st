*GT-SpotterExtensions-Core
spotterItemsFor: aStep
	<spotterOrder: 10>
	self hasSubMenu ifTrue: [ 
		self subMenu spotterItemsFor: aStep ]