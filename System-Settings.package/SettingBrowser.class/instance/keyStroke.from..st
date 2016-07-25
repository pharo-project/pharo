menu
keyStroke: anEvent from: aTreeMorph
	| c |
	c := anEvent keyCharacter.
	c = $a
		ifTrue: [^ self expandAll].
	c = $A
		ifTrue: [^ self collapseAll]. 
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph.
			current settingDeclaration hasDefault
				ifTrue: [c = $d
						ifTrue: [^ self menuSetToDefault: current]].
			current settingDeclaration hasEditableList
				ifTrue: [c = $e
						ifTrue: [^ self menuEmptyList: current]]]