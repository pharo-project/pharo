shortcuts
buildSimpleHierarchicalListShortcuts: aBuilder
	<keymap>
	(aBuilder shortcut: #cursorUp)
		category: #SimpleHierarchicalList
		default: Character arrowUp asKeyCombination
		do: [ :target | target setSelectionIndex: (target getSelectionIndex - 1 max: 1) ].
	(aBuilder shortcut: #cursorDown)
		category: #SimpleHierarchicalList
		default: Character arrowDown asKeyCombination
		do: [ :target | target setSelectionIndex: target getSelectionIndex + 1 ].
	(aBuilder shortcut: #cursorHome)
		category: #SimpleHierarchicalList
		default: Character home asKeyCombination
		do: [ :target | target setSelectionIndex: 1 ].
	(aBuilder shortcut: #cursorEnd)
		category: #SimpleHierarchicalList
		default: Character end asKeyCombination
		do: [ :target | target setSelectionIndex: target scroller submorphs size ].
	(aBuilder shortcut: #cursorPageUp)
		category: #SimpleHierarchicalList
		default: Character pageUp asKeyCombination
		do: [ :target | target setSelectionIndex: (target getSelectionIndex - target numSelectionsInView max: 1) ].
	(aBuilder shortcut: #cursorPageDown)
		category: #SimpleHierarchicalList
		default: Character pageDown asKeyCombination
		do: [ :target | target setSelectionIndex: target getSelectionIndex + target numSelectionsInView ].
	(aBuilder shortcut: #cursorRight)
		category: #SimpleHierarchicalList
		default: Character arrowRight asKeyCombination
		do: [ :target | 
			target selectedMorph
				ifNotNil: [ 
					target selectedMorph canExpand
						ifTrue: [ 
							target selectedMorph isExpanded not
								ifTrue: [ target toggleExpandedState: target selectedMorph ]
								ifFalse: [ target setSelectionIndex: target getSelectionIndex + 1 ] ] ] ].
	(aBuilder shortcut: #cursorLeft)
		category: #SimpleHierarchicalList
		default: Character arrowLeft asKeyCombination
		do: [ :target | 
			| selectedMorph |
			(selectedMorph := target selectedMorph)
				ifNotNil: [ 
					selectedMorph isExpanded
						ifTrue: [ target toggleExpandedState: selectedMorph ]
						ifFalse: [ 
							| i |
							"Search parent."
							selectedMorph indentLevel > 0
								ifTrue: [ 
									i := target getSelectionIndex max: 1.
									[ i > 1 and: [ (target scroller submorphs at: i) indentLevel >= selectedMorph indentLevel ] ]
										whileTrue: [ i := i - 1 max: 1 ].
									target setSelectionIndex: i ] ] ] ]