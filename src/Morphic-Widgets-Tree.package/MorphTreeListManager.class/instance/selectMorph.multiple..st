selection change
selectMorph: aNodeMorph multiple: withMultipleSelection 
	| path mult |
	self lastClickedMorph: aNodeMorph.
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
			self addToSelection: aNodeMorph.
			path := aNodeMorph path collect: [:m | m complexContents]].
	self selectionChanged.
	^ path