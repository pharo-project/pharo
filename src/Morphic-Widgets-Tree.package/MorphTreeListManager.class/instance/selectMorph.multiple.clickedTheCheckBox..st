selection change
selectMorph: aNodeMorph multiple: withMultipleSelection clickedTheCheckBox: checkBox
	| path mult |
	
	checkBox ifFalse: [ self lastClickedMorph: aNodeMorph ].
	aNodeMorph isNil
		ifTrue: [
			self emptySelection.
			^ nil].
	aNodeMorph selected
		ifTrue: [
			withMultipleSelection
				ifTrue: [self removeFromSelection: aNodeMorph]
				ifFalse: [
					mult := self selectedMorphList size > 1.
					path := aNodeMorph path collect: [:m | m complexContents].
					(self autoDeselection or: [mult])
						ifTrue: [
							self emptySelection.
							mult 
								ifTrue: [self addToSelection: aNodeMorph]
								ifFalse: [path := nil]]]]
		ifFalse: [
			withMultipleSelection
				ifFalse: [self emptySelection].
			self addToSelection: aNodeMorph clickedTheCheckBox: checkBox.
			path := aNodeMorph path collect: [:m | m complexContents]].
	self selectionChanged.
	^ path