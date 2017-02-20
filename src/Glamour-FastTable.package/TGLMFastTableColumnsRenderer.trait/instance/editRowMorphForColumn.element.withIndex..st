rendering
editRowMorphForColumn: glamourColumn element: element withIndex: aRowIndex 
	| brick |
	
	(brick := GLMEditableLabelBrick new)
		cancelOnEscape;
		cancelOnFocusLost;
		text: (self valueFor: element in: glamourColumn);
		hSpaceFill;
		with: [ :b | b editBrick rubricBackgroundColor: b themer rubricBackgroundColor ];
		when: #onSingleClicked do: [ :event :b | b isReadMode ifTrue: [ self table selectRowIndex: aRowIndex ] ];
		when: #onAccepted do: [ 
			glamourColumn modifiedBlock
				cull: brick text
				cull: element
				cull: self glamourPresentation ];
		yourself.
			
	^ brick asMorph
		hSpaceFill;
		vShrinkWrap