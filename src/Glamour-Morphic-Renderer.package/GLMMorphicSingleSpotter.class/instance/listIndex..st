accessing-list
listIndex: anInteger
	anInteger isZero not ifTrue: [
		parent selectionIn: self.].
	listIndex = anInteger
		ifTrue: [ ^ self ].
	listIndex := anInteger.
	self listSelection ifNotNil: [ :selection |
		self isAssisted ifTrue: [ 
			text := self model labelFor: self listSelection.
			self changed: #text ] ].
	self changed: #listIndex; changed: #isEnabled