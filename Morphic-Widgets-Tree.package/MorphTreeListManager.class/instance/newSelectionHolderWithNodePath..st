selection change
newSelectionHolderWithNodePath: aNodePath 
	^ self isMultiple
		ifTrue: [MorphTreeMorphMultipleSelection new
				selectedNodePathList: (self selectedMorphList
						collect: [:s | s path	collect: [:m | m complexContents]])] 
		ifFalse: [MorphTreeMorphSingleSelection new selectedNodePath: aNodePath]