accessing
selectedElement
	self tree hasSelectedItem
		ifFalse: [ ^ nil ].
	^ self tree selectedItem content