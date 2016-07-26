events handling
keyPressedOnList: anEvent shifted: aBoolean
	(anEvent keyCharacter == self class nextFocusKey
		and: [ (self packageWidget canExpand not or: [ self packageWidget isExpanded ]) ]) 
		ifTrue: [  
			self giveFocusTo: self classWidget.
			^ true ].
	
		^ false
