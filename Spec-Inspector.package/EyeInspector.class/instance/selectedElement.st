accessing
selectedElement
	^ self list selectedItem ifNil: [ 
		"sometimes the new list model desynchronize its selected index from its selected element,
		this mean that the inspector evaluates 'list setSelectedIndex: 1' but 'list selectedItem' still returns nil.
		This code fixes this case, that is triggered for example when you do Cmd+c in an ObjectExplorer."
		self list selectedIndex ifNotNil: [ :currentIndex | 
			currentIndex = 0
				ifTrue: [ ^ nil ].
			self list listItems ifNotEmpty: [ :aList | 
				aList at: currentIndex  ] ] ]