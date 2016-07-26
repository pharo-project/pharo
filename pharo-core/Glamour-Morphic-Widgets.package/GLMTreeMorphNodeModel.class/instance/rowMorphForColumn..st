callbacks
rowMorphForColumn: aGlamourColumn
	| rowElements contentMorph tagMorphs |
		
	aGlamourColumn modifiedBlock
		ifNil: [
			contentMorph := (self containerTree glamourPresentation 
			column: aGlamourColumn
			valueFor: self item) asMorph asReadOnlyMorph ]
		ifNotNil: [
			contentMorph := GLMEditableLabelBrick new
				cancelOnEscape;
				cancelOnFocusLost;
				text: (self valueFor: aGlamourColumn);
				hSpaceFill;
				with: [ :b | b editBrick rubricBackgroundColor: b themer rubricBackgroundColor ];
				when: #onEditMode do: [ self containerTree forceStopStepping ];
				when: #onReadMode do: [ self containerTree resetStopStepping ];
				when: #onSingleClicked do: [  :evt :brick | brick isReadMode ifTrue: [
						(brick ownerThatIsA: GLMPaginatedResizableMorphTreeMorph) 
							listManager mouseUp: evt on: brick owner owner owner.

						"self containerTree 
							deselectAll;
							selectNodePath: self path" ] ];
				when: #onAccepted do: [ self modified: aGlamourColumn with: contentMorph text ];
				yourself.
			
			contentMorph := contentMorph asMorph
				hSpaceFill;
				vShrinkWrap ].
	
	tagMorphs := self tagMorphsFrom: aGlamourColumn.
	tagMorphs ifEmpty: [ ^ contentMorph ].
	
	rowElements := OrderedCollection new
		add: contentMorph;
		addAll: tagMorphs;
		yourself.			
	^ (UITheme current newRowIn: World for: rowElements)