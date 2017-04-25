rendering
editRowMorphForColumn: glamourColumn element: element withIndex: aRowIndex 
	| rowMorph |

	rowMorph := self viewRowMorphForColumn: glamourColumn element: element withIndex: aRowIndex.
	"rowMorph hResizing: #spaceFill.
	rowMorph
		on: #mouseDown
		send: #value:
		to: [ :event | 
			event shiftPressed ifTrue: [
				GLMEditorPopupBrick new
					closer: (GLMPopupBrickCloser new
						predicate: GLMPopupBrickOutisideClickPredicate new;
						predicate: GLMPopupBrickEscPredicate new;
						yourself);
					textModel: (self valueFor: element withIndex: aRowIndex in: glamourColumn);
					openOn: rowMorph ]
				ifFalse: [ event wasHandled: false ] ]."
	
	^ rowMorph
