rendering
editRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex 
	| rowMorph |

	rowMorph := self viewRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex.
	rowMorph hResizing: #spaceFill.
	rowMorph
		on: #mouseDown
		send: #value:
		to: [ :event | 
			event shiftPressed ifTrue: [
				GLMEditorPopupBrick new
					in: [ :aPopup | aPopup themer: aPopup themer popupEditorBorderThemer themer ];
					closer: (GLMPopupBrickCloser new
						predicate: GLMPopupBrickOutisideClickPredicate new;
						predicate: GLMPopupBrickEscPredicate new;
						yourself);
					textModel: (self valueFor: aNode withIndex: aRowIndex in: glamourColumn);
					when: #textAccepted do: [ :anEvent | glamourColumn modifiedBlock
						cull: anEvent morph text
						cull: aNode
						cull: self glamourPresentation ];
					openOn: rowMorph ]
				ifFalse: [ event wasHandled: false ] ].
	
	^ rowMorph
