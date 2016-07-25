change reporting
ownerChanged
	| priorEditor |
	super ownerChanged.
	container ifNotNil: 
			[editor isNil 
				ifTrue:
					[self releaseParagraph.
					(container isKindOf: TextContainer) ifTrue:
						["May need to recompose due to changes in owner"
						self installEditorToReplace: nil.
						self releaseParagraph]]
				ifFalse: 
					[priorEditor := editor.
					self releaseParagraph.
					self installEditorToReplace: priorEditor]]