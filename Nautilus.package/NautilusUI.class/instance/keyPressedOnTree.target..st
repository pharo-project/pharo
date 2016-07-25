events handling
keyPressedOnTree: anEvent target: aMorph 
	(anEvent keyCharacter == self class nextFocusKey
		and: [ (aMorph canExpand not or: [ aMorph isExpanded ]) ]) 
		ifTrue: [  
			self giveFocusTo: self classWidget.
			^ true ].
	
		^ false
