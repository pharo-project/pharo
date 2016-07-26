factory
editRowMorphForColumn: glamourColumn element: element
	| brick |
	
	(brick := GLMEditableLabelBrick new)
		cancelOnEscape;
		cancelOnFocusLost;
		text: (self valueFor: glamourColumn);
		hSpaceFill;
		with: [ :b | b editBrick rubricBackgroundColor: b themer rubricBackgroundColor ];
		"when: #onEditMode do: [ self containerTree forceStopStepping ];
		when: #onReadMode do: [ self containerTree resetStopStepping ];"
		"when: #onSingleClicked do: [ :event :b | b isReadMode ifTrue: [ self selectItem ] ];"
		"when: #onAccepted do: [ self modified: glamourColumn with: brick text ];"
		yourself.
			
	^ brick asMorph
		hSpaceFill;
		vShrinkWrap