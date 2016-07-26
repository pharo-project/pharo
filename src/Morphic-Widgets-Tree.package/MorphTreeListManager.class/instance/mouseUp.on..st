mouse managing
mouseUp: event on: aNodeMorph 
	| path cmdOrCtrl clickedTheCheckbox |
	"No change if model is locked" 
	
	clickedTheCheckbox := self clicked: event inTheCheckboxOf: aNodeMorph.
	self listModel okToChange ifFalse: [^self].
	self listModel okToDiscardEdits ifFalse: [^ self].

	cmdOrCtrl := self commandOrCrontrolKeyPressed: event.
	
	path := (event shiftPressed and: [self isMultiple])
				ifTrue: [self shiftSelectMorph: aNodeMorph]
				ifFalse: [
					self autoMultiSelection
						ifTrue: [self selectedMorphList ifEmpty: [] ifNotEmpty: [:l | l last path collect: [:p | p complexContents]]]
						ifFalse: [self selectMorph: aNodeMorph multiple: ((cmdOrCtrl and: [self isMultiple]) or: [self autoMultiSelection]) clickedTheCheckBox: clickedTheCheckbox ]].
	self selectionUpdateFromViewWhile: [ | selHolder | 
		selHolder := self newSelectionHolderWithNodePath: path.
		self listModel selection: selHolder].
	
	clickedTheCheckbox ifFalse: [ self lastClickedMorph: aNodeMorph ].
	self selectionChanged. 
